{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Solana PubSub (WebSocket) protocol layer.
--
-- The request goldens and notification payloads below are the canonical
-- examples from the Solana WebSocket RPC documentation, so they pin this
-- module against the documented wire format rather than against our own
-- encoder's habits.
module Test.RPC.WebSocket (tests) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Network.Solana.Core.Account (AccountData (..), AccountInfo (..), Lamport (..))
import Network.Solana.Core.Crypto (mkPublicKeyFromString, unsafeSigFromString)
import Network.Solana.RPC.WebSocket
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Network.Solana.RPC.WebSocket"
    [ testGroup "requests" requestTests,
      testGroup "incoming messages" parseTests,
      testGroup "awaitSignature" awaitTests
    ]

-- | The signature used throughout the Solana WebSocket documentation.
docSignature :: String
docSignature = "2EBVM6cB8vAAD93Ktr6Vd8p67XPbQzCJX47MpReuiCXJAtcjaxpvWpcg9Ege1Nr5Tk3a2GFrByT7WPBjdsTycY9b"

-- | The account pubkey used in the documented @accountSubscribe@ example.
docPubkey :: String
docPubkey = "CM78CPUeXjn8o3yroDHxUtKsZZgoy4GPkPPXfouKNH12"

requestTests :: [TestTree]
requestTests =
  [ testCase "signatureSubscribe matches the documented request" $
      signatureSubscribeRequest (RequestId 1) (unsafeSigFromString docSignature) (Just "finalized") False
        @?= BS8.pack
          ( "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"signatureSubscribe\",\"params\":[\""
              <> docSignature
              <> "\",{\"commitment\":\"finalized\",\"enableReceivedNotification\":false}]}"
          ),
    testCase "signatureSubscribe omits commitment when unset and honours the received flag" $
      signatureSubscribeRequest (RequestId 7) (unsafeSigFromString docSignature) Nothing True
        @?= BS8.pack
          ( "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"signatureSubscribe\",\"params\":[\""
              <> docSignature
              <> "\",{\"enableReceivedNotification\":true}]}"
          ),
    testCase "accountSubscribe matches the documented request" $ do
      pk <- either assertFailure' pure (mkPublicKeyFromString docPubkey)
      accountSubscribeRequest (RequestId 1) pk (Just "finalized")
        @?= BS8.pack
          ( "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"accountSubscribe\",\"params\":[\""
              <> docPubkey
              <> "\",{\"commitment\":\"finalized\",\"encoding\":\"base64\"}]}"
          ),
    testCase "signatureUnsubscribe matches the documented request" $
      signatureUnsubscribeRequest (RequestId 1) (SubscriptionId 0)
        @?= "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"signatureUnsubscribe\",\"params\":[0]}",
    testCase "accountUnsubscribe matches the documented request" $
      accountUnsubscribeRequest (RequestId 1) (SubscriptionId 0)
        @?= "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"accountUnsubscribe\",\"params\":[0]}"
  ]

parseTests :: [TestTree]
parseTests =
  [ testCase "subscribe acknowledgement carries the subscription id" $
      parseWsMessage "{\"jsonrpc\":\"2.0\",\"result\":23784,\"id\":1}"
        @?= Right (SubscribeAck (RequestId 1) (SubscriptionId 23784)),
    testCase "unsubscribe acknowledgement is distinguished by its boolean result" $
      parseWsMessage "{\"jsonrpc\":\"2.0\",\"result\":true,\"id\":1}"
        @?= Right (UnsubscribeAck (RequestId 1) True),
    -- PubSub omits context.apiVersion, unlike the HTTP RPC responses that
    -- 'Network.Solana.RPC.HTTP.Types.Context' models; parsing must not
    -- require it.
    testCase "signature notification without context.apiVersion parses as success" $
      parseWsMessage
        "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5207624},\"value\":{\"err\":null}},\"subscription\":24006}}"
        @?= Right (SignatureNotice (SubscriptionId 24006) (SignatureNotification 5207624 Nothing False)),
    testCase "signature notification surfaces an on-chain error" $
      case parseWsMessage
        "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":10},\"value\":{\"err\":{\"InstructionError\":[0,{\"Custom\":1}]}}},\"subscription\":1}}" of
        Right (SignatureNotice sub n) -> do
          sub @?= SubscriptionId 1
          snSlot n @?= 10
          snReceived n @?= False
          assertBool "error preserved" (isJust (snErr n))
        other -> assertFailure ("expected a signature notification, got " <> show other),
    testCase "receivedSignature notification is flagged, not treated as terminal" $
      parseWsMessage
        "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":9},\"value\":\"receivedSignature\"},\"subscription\":2}}"
        @?= Right (SignatureNotice (SubscriptionId 2) (SignatureNotification 9 Nothing True)),
    testCase "account notification decodes into the shared AccountInfo type" $
      case parseWsMessage
        "{\"jsonrpc\":\"2.0\",\"method\":\"accountNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5199307},\"value\":{\"data\":[\"AQID\",\"base64\"],\"executable\":false,\"lamports\":33594,\"owner\":\"11111111111111111111111111111111\",\"rentEpoch\":635,\"space\":3}},\"subscription\":23784}}" of
        Right (AccountNotice sub n) -> do
          sub @?= SubscriptionId 23784
          anSlot n @?= 5199307
          lamports (anAccount n) @?= Lamport 33594
          executable (anAccount n) @?= False
          dataField (anAccount n) @?= AccountDataBinary (BS8.pack "\SOH\STX\ETX")
        other -> assertFailure ("expected an account notification, got " <> show other),
    testCase "JSON-RPC error responses are reported, not silently dropped" $
      parseWsMessage "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32602,\"message\":\"Invalid Request\"},\"id\":3}"
        @?= Right (WsErrorMessage (Just (RequestId 3)) (-32602) "Invalid Request"),
    testCase "JSON-RPC error with a null id is still reported" $
      parseWsMessage "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"Parse error\"},\"id\":null}"
        @?= Right (WsErrorMessage Nothing (-32700) "Parse error"),
    testCase "malformed JSON is rejected" $
      assertBool "expected a Left" (isLeft (parseWsMessage "not json")),
    testCase "an unknown notification method is rejected rather than misread" $
      assertBool
        "expected a Left"
        (isLeft (parseWsMessage "{\"jsonrpc\":\"2.0\",\"method\":\"slotNotification\",\"params\":{\"result\":{\"slot\":1},\"subscription\":1}}")),
    -- The node always sends err (null on success); a value without it is
    -- malformed and must not read as a success.
    testCase "signature notification value without an err key is rejected" $
      assertBool
        "expected a Left"
        (isLeft (parseWsMessage "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5},\"value\":{}},\"subscription\":1}}"))
  ]

-- | A 'WsTransport' that replays a fixed script of incoming frames and
-- records everything sent, so the confirmation handshake can be driven
-- without a network. Receiving past the end of the script blocks forever,
-- which is how the timeout case is exercised.
scriptedTransport :: [BS.ByteString] -> IO (WsTransport, IORef [BS.ByteString])
scriptedTransport incoming = do
  remaining <- newIORef incoming
  sent <- newIORef []
  let recv = do
        next <- atomicModifyIORef' remaining (\case [] -> ([], Nothing); (y : ys) -> (ys, Just y))
        maybe (newEmptyMVar >>= takeMVar) pure next
      send frame = modifyIORef' sent (<> [frame])
  pure (WsTransport send recv, sent)

ack :: BS.ByteString
ack = "{\"jsonrpc\":\"2.0\",\"result\":24006,\"id\":1}"

terminalNotice :: BS.ByteString
terminalNotice =
  "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5207624},\"value\":{\"err\":null}},\"subscription\":24006}}"

awaitTests :: [TestTree]
awaitTests =
  [ testCase "returns the confirming slot and sends the documented subscribe request" $ do
      (transport, sent) <- scriptedTransport [ack, terminalNotice]
      result <- awaitSignature transport (RequestId 1) (Just "confirmed") 5 (unsafeSigFromString docSignature)
      result @?= Right 5207624
      frames <- readIORef sent
      frames @?= [signatureSubscribeRequest (RequestId 1) (unsafeSigFromString docSignature) (Just "confirmed") False],
    testCase "reports a transaction that failed on-chain" $ do
      (transport, _) <- scriptedTransport
        [ ack,
          "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":11},\"value\":{\"err\":{\"InstructionError\":[0,{\"Custom\":1}]}}},\"subscription\":24006}}"
        ]
      result <- awaitSignature transport (RequestId 1) Nothing 5 (unsafeSigFromString docSignature)
      assertBool ("expected an on-chain failure, got " <> show result) (either ("failed on-chain" `isInfixOf`) (const False) result),
    testCase "skips other subscriptions' frames and the early receivedSignature" $ do
      (transport, _) <- scriptedTransport
        [ -- a notification for a subscription we never made
          "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":1},\"value\":{\"err\":null}},\"subscription\":999}}",
          -- an acknowledgement for somebody else's request
          "{\"jsonrpc\":\"2.0\",\"result\":31337,\"id\":42}",
          ack,
          -- our own early received notification: seen, but not terminal
          "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5207600},\"value\":\"receivedSignature\"},\"subscription\":24006}}",
          terminalNotice
        ]
      result <- awaitSignature transport (RequestId 1) (Just "confirmed") 5 (unsafeSigFromString docSignature)
      result @?= Right 5207624,
    testCase "reports a subscription rejected by the node" $ do
      (transport, _) <- scriptedTransport ["{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32602,\"message\":\"Invalid Request\"},\"id\":1}"]
      result <- awaitSignature transport (RequestId 1) Nothing 5 (unsafeSigFromString docSignature)
      assertBool ("expected a subscription failure, got " <> show result) (either ("subscription failed" `isInfixOf`) (const False) result),
    testCase "reports an unparseable frame instead of hanging" $ do
      (transport, _) <- scriptedTransport ["not json"]
      result <- awaitSignature transport (RequestId 1) Nothing 5 (unsafeSigFromString docSignature)
      assertBool ("expected a parse failure, got " <> show result) (either (const True) (const False) result),
    testCase "times out when the node never answers" $ do
      (transport, _) <- scriptedTransport []
      result <- awaitSignature transport (RequestId 1) Nothing 1 (unsafeSigFromString docSignature)
      assertBool ("expected a timeout, got " <> show result) (either ("timed out" `isInfixOf`) (const False) result),
    testCase "does not report success for a notification lacking err" $ do
      (transport, _) <-
        scriptedTransport
          [ ack,
            "{\"jsonrpc\":\"2.0\",\"method\":\"signatureNotification\",\"params\":{\"result\":{\"context\":{\"slot\":5},\"value\":{}},\"subscription\":24006}}"
          ]
      result <- awaitSignature transport (RequestId 1) Nothing 5 (unsafeSigFromString docSignature)
      assertBool ("expected a failure, got " <> show result) (either (const True) (const False) result)
  ]

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

assertFailure' :: String -> IO a
assertFailure' = assertFailure
