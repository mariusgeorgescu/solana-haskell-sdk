{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.WebSocket
-- Description : Solana PubSub (WebSocket) subscriptions and push-based confirmation.
--
-- Solana nodes expose a PubSub endpoint alongside the JSON-RPC one (by
-- convention port @8900@ next to the RPC port @8899@; @wss://@ on hosted
-- clusters). Instead of polling for a transaction's status, a client
-- subscribes once and is pushed a notification the moment the signature
-- reaches the requested commitment.
--
-- This module is transport-agnostic on purpose: it builds the exact bytes to
-- send, parses the bytes received, and drives the confirmation handshake over
-- a 'WsTransport' that the caller supplies. The SDK therefore needs no
-- WebSocket dependency of its own, and the same code works against plain
-- @ws://@ and TLS @wss://@ endpoints.
--
-- Wiring it to the @websockets@ package takes a few lines:
--
-- > import Network.WebSockets qualified as WS
-- >
-- > WS.runClient "127.0.0.1" 8900 "/" $ \conn -> do
-- >   let transport = WsTransport (WS.sendTextData conn) (WS.receiveData conn)
-- >   result <- awaitSignature transport (RequestId 1) (Just "confirmed") 30 signature
-- >   print result
--
-- For @wss://@ endpoints, use @wuss@'s @runSecureClient@ in place of
-- @runClient@; nothing else changes.
module Network.Solana.RPC.WebSocket
  ( -- * Identifiers
    RequestId (..),
    SubscriptionId (..),

    -- * Requests
    signatureSubscribeRequest,
    accountSubscribeRequest,
    signatureUnsubscribeRequest,
    accountUnsubscribeRequest,

    -- * Incoming messages
    WsMessage (..),
    SignatureNotification (..),
    AccountNotification (..),
    parseWsMessage,

    -- * Transport
    WsTransport (..),

    -- * Confirmation
    awaitSignature,
  )
where

import Data.Aeson
import Data.Aeson.Encoding qualified as E
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Data.Word (Word64)
import Network.Solana.Core.Account (AccountInfo)
import Network.Solana.Core.Crypto (SolanaPublicKey, SolanaSignature)
import Network.Solana.RPC.HTTP.Types (Slot)
import System.Timeout (timeout)

------------------------------------------------------------------------------------------------

-- * Identifiers

------------------------------------------------------------------------------------------------

-- | A client-supplied JSON-RPC request id. The node echoes it in the
-- subscription acknowledgement, which is how a reply is matched to its
-- request.
newtype RequestId = RequestId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

-- | A server-assigned subscription id, returned by a @*Subscribe@ call and
-- carried by every notification belonging to that subscription. Pass it to
-- the matching @*Unsubscribe@ request to cancel early.
newtype SubscriptionId = SubscriptionId Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

------------------------------------------------------------------------------------------------

-- * Requests

------------------------------------------------------------------------------------------------

-- | Encodes a JSON-RPC request with its fields in the documented order.
encodeRequest :: RequestId -> Text -> E.Encoding -> BS.ByteString
encodeRequest (RequestId i) method params =
  BL.toStrict . E.encodingToLazyByteString . E.pairs $
    E.pair "jsonrpc" (E.text "2.0")
      <> E.pair "id" (E.word64 i)
      <> E.pair "method" (E.text method)
      <> E.pair "params" params

-- | Optional @commitment@ field: omitted entirely when unset, in which case
-- the node applies its default (@finalized@).
commitmentField :: Maybe String -> E.Series
commitmentField = foldMap (E.pair "commitment" . E.string)

-- | A @signatureSubscribe@ request for the given signature.
--
-- The subscription ends by itself once the signature reaches the requested
-- commitment, so a successful wait needs no unsubscribe. When the last
-- argument is 'True', the node may additionally send an early
-- @receivedSignature@ notification (see 'snReceived').
signatureSubscribeRequest :: RequestId -> SolanaSignature -> Maybe String -> Bool -> BS.ByteString
signatureSubscribeRequest reqId sig mCommitment enableReceived =
  encodeRequest reqId "signatureSubscribe" $
    E.list
      id
      [ E.string (show sig),
        E.pairs (commitmentField mCommitment <> E.pair "enableReceivedNotification" (E.bool enableReceived))
      ]

-- | An @accountSubscribe@ request for the given account.
--
-- Account data is requested @base64@-encoded, matching
-- 'Network.Solana.RPC.HTTP.Account.getAccountInfo', so every state decoder in
-- this library applies unchanged to 'anAccount'.
accountSubscribeRequest :: RequestId -> SolanaPublicKey -> Maybe String -> BS.ByteString
accountSubscribeRequest reqId pk mCommitment =
  encodeRequest reqId "accountSubscribe" $
    E.list
      id
      [ E.string (show pk),
        E.pairs (commitmentField mCommitment <> E.pair "encoding" (E.string "base64"))
      ]

-- | A @signatureUnsubscribe@ request cancelling the given subscription.
signatureUnsubscribeRequest :: RequestId -> SubscriptionId -> BS.ByteString
signatureUnsubscribeRequest reqId (SubscriptionId s) =
  encodeRequest reqId "signatureUnsubscribe" (E.list id [E.word64 s])

-- | An @accountUnsubscribe@ request cancelling the given subscription.
accountUnsubscribeRequest :: RequestId -> SubscriptionId -> BS.ByteString
accountUnsubscribeRequest reqId (SubscriptionId s) =
  encodeRequest reqId "accountUnsubscribe" (E.list id [E.word64 s])

------------------------------------------------------------------------------------------------

-- * Incoming messages

------------------------------------------------------------------------------------------------

-- | The status of a subscribed signature.
data SignatureNotification = SignatureNotification
  { -- | The slot the notification is valid for.
    snSlot :: Slot,
    -- | 'Nothing' when the transaction succeeded at the subscribed
    -- commitment; the node's @TransactionError@ otherwise.
    snErr :: Maybe Value,
    -- | 'True' for the early @receivedSignature@ notification, which reports
    -- that the node has seen the transaction but says nothing about whether
    -- it succeeded. Such a notification is not terminal: the subscription
    -- stays open until the signature reaches the requested commitment.
    snReceived :: Bool
  }
  deriving (Eq, Show)

-- | A subscribed account's state after a change.
data AccountNotification = AccountNotification
  { -- | The slot the notification is valid for.
    anSlot :: Slot,
    -- | The account as of that slot, shaped exactly like a @getAccountInfo@
    -- value.
    anAccount :: AccountInfo
  }
  deriving (Eq, Show)

-- | A message received from a Solana PubSub endpoint.
data WsMessage
  = -- | A @*Subscribe@ request succeeded, yielding a subscription id.
    SubscribeAck RequestId SubscriptionId
  | -- | An @*Unsubscribe@ request completed.
    UnsubscribeAck RequestId Bool
  | -- | A @signatureNotification@ for an active subscription.
    SignatureNotice SubscriptionId SignatureNotification
  | -- | An @accountNotification@ for an active subscription.
    AccountNotice SubscriptionId AccountNotification
  | -- | A JSON-RPC error, with its code and message. The request id is
    -- 'Nothing' when the node could not attribute the error to a request.
    WsErrorMessage (Maybe RequestId) Int String
  deriving (Eq, Show)

-- | Parses one PubSub frame.
--
-- Note that PubSub notifications carry a @context@ object holding only a
-- @slot@ -- unlike the HTTP RPC responses modelled by
-- 'Network.Solana.RPC.HTTP.Types.Context', they omit @apiVersion@ -- so they
-- are parsed here rather than through @RPCResponse@.
parseWsMessage :: BS.ByteString -> Either String WsMessage
parseWsMessage raw = eitherDecodeStrict' raw >>= parseEither wsMessageParser

wsMessageParser :: Value -> Parser WsMessage
wsMessageParser = withObject "WsMessage" $ \o -> do
  mMethod <- o .:? "method"
  case mMethod :: Maybe Text of
    Just "signatureNotification" -> notice o SignatureNotice signatureNotificationParser
    Just "accountNotification" -> notice o AccountNotice accountNotificationParser
    Just other -> fail ("unsupported notification method: " <> Text.unpack other)
    Nothing -> do
      mError <- o .:? "error"
      case mError of
        Just err ->
          WsErrorMessage
            <$> o .:? "id"
            <*> err .: "code"
            <*> err .: "message"
        Nothing -> do
          reqId <- o .: "id"
          result <- o .: "result"
          case result of
            Bool ok -> pure (UnsubscribeAck reqId ok)
            Number _ -> SubscribeAck reqId . SubscriptionId <$> parseJSON result
            _ -> fail "result is neither a subscription id nor an unsubscribe flag"
  where
    notice o build parser = do
      params <- o .: "params"
      sub <- params .: "subscription"
      result <- params .: "result"
      build sub <$> parser result

-- | Both notification kinds wrap their payload in @{context: {slot}, value}@.
withNotificationContext :: (Slot -> Value -> Parser a) -> Value -> Parser a
withNotificationContext build = withObject "notification result" $ \result -> do
  ctx <- result .: "context"
  slot <- ctx .: "slot"
  value <- result .: "value"
  build slot value

signatureNotificationParser :: Value -> Parser SignatureNotification
signatureNotificationParser = withNotificationContext $ \slot value ->
  case value of
    String "receivedSignature" -> pure (SignatureNotification slot Nothing True)
    _ -> flip (withObject "signature notification value") value $ \v -> do
      err <- v .:? "err"
      pure (SignatureNotification slot err False)

accountNotificationParser :: Value -> Parser AccountNotification
accountNotificationParser = withNotificationContext $ \slot value ->
  AccountNotification slot <$> parseJSON value

------------------------------------------------------------------------------------------------

-- * Transport and confirmation

------------------------------------------------------------------------------------------------

-- | A minimal bidirectional message transport: everything this module needs
-- from a WebSocket connection.
--
-- Supplying this rather than a concrete connection type is what keeps the
-- library free of a WebSocket dependency (see the module header for the
-- @websockets@ wiring), and lets the confirmation handshake be tested without
-- a network.
data WsTransport = WsTransport
  { -- | Send one text frame.
    wsSend :: BS.ByteString -> IO (),
    -- | Receive one text frame, blocking until it arrives.
    wsReceive :: IO BS.ByteString
  }

-- | Subscribes to a signature and waits for its terminal notification,
-- returning the slot it was confirmed in.
--
-- This is the push-based counterpart to
-- 'Network.Solana.SolanaWeb3.confirmTransaction': the node sends the result
-- as soon as the signature reaches @commitment@ rather than being polled for
-- it. Because @signatureSubscribe@ cancels itself once that notification is
-- sent, a successful wait leaves nothing to clean up.
--
-- Frames that belong to other subscriptions, and early @receivedSignature@
-- notifications, are skipped. The whole exchange -- subscribe, acknowledge,
-- notify -- is bounded by @timeoutSeconds@.
--
-- Returns the confirming slot, or 'Left' describing a transaction that failed
-- on-chain, a JSON-RPC error, an unparseable frame, or the timeout. Exceptions
-- raised by the transport itself (a closed connection, for instance) are not
-- caught.
--
-- A wait that ends in a timeout, a bad frame, or an on-chain failure may leave
-- the subscription open, because only the terminal notification retires it.
-- That costs nothing if the connection is about to be closed, which is the
-- common case; a caller that keeps the connection alive should cancel it with
-- 'signatureUnsubscribeRequest'. Note also that this reads frames directly
-- from the transport, so one connection supports one wait at a time —
-- multiplexing several concurrent subscriptions needs a reader loop
-- dispatching on 'parseWsMessage', which this function deliberately is not.
awaitSignature :: WsTransport -> RequestId -> Maybe String -> Int -> SolanaSignature -> IO (Either String Slot)
awaitSignature transport reqId mCommitment timeoutSeconds sig = do
  result <- timeout (timeoutSeconds * 1000000) $ do
    wsSend transport (signatureSubscribeRequest reqId sig mCommitment False)
    subId <- awaitAck
    either (pure . Left) awaitNotice subId
  pure (fromMaybe (Left ("awaitSignature: timed out waiting for " <> show sig)) result)
  where
    -- Read until the acknowledgement for our own request id arrives.
    awaitAck = do
      msg <- next
      case msg of
        Left err -> pure (Left err)
        Right (SubscribeAck rid subId) | rid == reqId -> pure (Right subId)
        Right (WsErrorMessage rid code message)
          | rid == Just reqId || isNothing rid ->
              pure (Left ("awaitSignature: subscription failed (" <> show code <> "): " <> message))
        Right _ -> awaitAck

    -- Read until this subscription's terminal notification arrives.
    awaitNotice subId = do
      msg <- next
      case msg of
        Left err -> pure (Left err)
        Right (SignatureNotice sub n)
          | sub == subId,
            not (snReceived n) ->
              pure $ case snErr n of
                Nothing -> Right (snSlot n)
                Just err -> Left ("awaitSignature: transaction failed on-chain: " <> show err)
        Right _ -> awaitNotice subId

    next = either (Left . ("awaitSignature: " <>)) Right . parseWsMessage <$> wsReceive transport
