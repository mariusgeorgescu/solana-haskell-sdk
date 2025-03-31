{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Transaction where

import Core.Account (Lamport)
import Core.Crypto (SolanaPublicKey, SolanaSignature)
import Core.Transaction
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getFeeForMessage

------------------------------------------------------------------------------------------------

-- | Get the fee the network will charge for a particular Message.
-- Returns 'RpcResponse Lamport' with value field set  to the fee corresponding to the message at the specified blockhash
getFeeForMessage' :: (JsonRpc m) => String -> m (RPCResponse (Maybe Int))
getFeeForMessage' = do
  remote "getFeeForMessage"
{-# INLINE getFeeForMessage' #-}

-- | Get the fee the network will charge for a particular Message.
-- Returns the fee corresponding to the message at the specified blockhash
getFeeForMessage :: (JsonRpc m) => String -> m (Maybe Int)
getFeeForMessage = fmap value . getFeeForMessage'
{-# INLINE getFeeForMessage #-}

------------------------------------------------------------------------------------------------

-- * getTransaction

------------------------------------------------------------------------------------------------

-- | Returns information about the current supply.
getTransaction :: (JsonRpc m) => SolanaSignature -> m TransactionResult
getTransaction = do
  remote "getTransaction"

data TransactionResult = TransactionResult
  { -- | The slot this transaction was processed in.
    slotTx :: Slot,
    -- | Estimated production time, as Unix timestamp (seconds since the Unix epoch) of when the transaction was processed. 'Nothing' if not available.
    blockTimeTx :: Maybe Int64,
    -- | Transaction status metadata object or 'Nothing'.
    metaTx :: Maybe Object,
    -- | Transaction information.
    transactionTx :: Transaction
  }
  deriving (Generic, Show)

instance FromJSON TransactionResult where
  parseJSON :: Value -> Parser TransactionResult
  parseJSON = withObject "TransactionResult" $ \v ->
    TransactionResult
      <$> v .: "slot"
      <*> v .: "blockTime"
      <*> (v .: "meta")
      <*> v .: "transaction"

------------------------------------------------------------------------------------------------

-- * requestAirdrop

------------------------------------------------------------------------------------------------

requestAirdrop :: (JsonRpc m) => SolanaPublicKey -> Lamport -> m SolanaSignature
requestAirdrop = do
  remote "requestAirdrop"

------------------------------------------------------------------------------------------------

-- * sendTransaction

------------------------------------------------------------------------------------------------

defaultRpcSendTransactionConfig :: ConfigurationObject
defaultRpcSendTransactionConfig =
  defaultConfigObject
    { encoding = Just "base64",
      skipPreflight = Just True,
      preflightCommitment = Just "finalized",
      maxRetries = Just 0,
      minContextSlot = Just 0
    }

sendTransaction :: (JsonRpc m) => String -> m String
sendTransaction tx = sendTransaction' tx defaultRpcSendTransactionConfig

sendTransaction' :: (JsonRpc m) => String -> ConfigurationObject -> m String
sendTransaction' = do
  remote "sendTransaction"

------------------------------------------------------------------------------------------------

-- * simulateTransaction

------------------------------------------------------------------------------------------------
simulateTransaction' :: (JsonRpc m) => String -> ConfigurationObject -> m Object
simulateTransaction' = do
  remote "simulateTransaction"

simulateTransaction :: (JsonRpc m) => String -> m Object
simulateTransaction tx = simulateTransaction' tx cfgJustEncodingBase64
