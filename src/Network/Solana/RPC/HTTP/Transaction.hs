{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Transaction
-- Description : Solana RPC methods for submitting, inspecting, and simulating transactions.
--
-- This module provides Solana JSON-RPC bindings related to transaction lifecycle operations:
-- fee estimation, transaction broadcasting, simulation, airdrops (on devnet), and retrieving
-- metadata for finalized transactions.
--
-- These endpoints are essential for any client application that signs and sends transactions.
module Network.Solana.RPC.HTTP.Transaction where

import Data.Aeson
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto (SolanaPublicKey, SolanaSignature)
import Network.Solana.Core.Transaction
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getFeeForMessage

------------------------------------------------------------------------------------------------

-- | Returns the fee the network will charge for a particular message (base64-encoded).
-- Returns 'RPCResponse' ('Maybe' 'Int') where the value is the fee in lamports, or 'Nothing' if the fee couldn't be calculated.
getFeeForMessage' :: (JsonRpc m) => String -> m (RPCResponse (Maybe Int))
getFeeForMessage' = do
  remote "getFeeForMessage"
{-# INLINE getFeeForMessage' #-}

-- | Returns the fee the network will charge for a particular message (base64-encoded).
-- Returns 'Maybe Int' fee in lamports.
getFeeForMessage :: (JsonRpc m) => String -> m (Maybe Int)
getFeeForMessage = fmap value . getFeeForMessage'
{-# INLINE getFeeForMessage #-}

------------------------------------------------------------------------------------------------

-- * getTransaction

------------------------------------------------------------------------------------------------

-- | Returns metadata and content for a confirmed transaction identified by
-- its signature, using the given configuration.
-- Returns 'Nothing' if the signature is unknown or has expired.
getTransaction' :: (JsonRpc m) => SolanaSignature -> ConfigurationObject -> m (Maybe TransactionResult)
getTransaction' = do
  remote "getTransaction"
{-# INLINE getTransaction' #-}

-- | Returns metadata and content for a confirmed transaction identified by
-- its signature. Sets @maxSupportedTransactionVersion@ to 1, so that v0 and
-- v1 (SIMD-0385) transactions can be decoded -- without a high enough value
-- the node responds with error -32015.
--
-- Versioned transactions are returned without the message parts
-- t'Network.Solana.Core.Message.CompiledMessage' does not model: a v0
-- transaction's address-lookup-table data (so instruction account indices
-- may reference addresses not present in the parsed account keys), and a v1
-- transaction's @transactionConfig@ resource requests (see
-- 'Network.Solana.RPC.HTTP.Block.getBlock').
-- Returns 'Nothing' if the signature is unknown or has expired.
getTransaction :: (JsonRpc m) => SolanaSignature -> m (Maybe TransactionResult)
getTransaction sig = getTransaction' sig (defaultConfigObject {encoding = Just "json", maxSupportedTransactionVersion = Just 1})
{-# INLINE getTransaction #-}

-- | Contains metadata and transaction information returned by 'getTransaction'.
data TransactionResult = TransactionResult
  { -- | The slot this transaction was processed in.
    slotTx :: Slot,
    -- | Estimated production time, as Unix timestamp (seconds since the Unix epoch) when the transaction was processed.
    blockTimeTx :: Maybe Int64,
    -- | Optional transaction metadata.
    metaTx :: Maybe Object,
    -- | The transaction content.
    transactionTx :: Transaction
  }
  deriving (Generic, Show)

instance FromJSON TransactionResult where
  parseJSON = withObject "TransactionResult" $ \v ->
    TransactionResult
      <$> v .: "slot"
      <*> v .: "blockTime"
      <*> v .: "meta"
      <*> v .: "transaction"

------------------------------------------------------------------------------------------------

-- * requestAirdrop

------------------------------------------------------------------------------------------------

-- | Requests an airdrop of the specified number of lamports to the given address.
requestAirdrop :: (JsonRpc m) => SolanaPublicKey -> Lamport -> m SolanaSignature
requestAirdrop = do
  remote "requestAirdrop"
{-# INLINE requestAirdrop #-}

------------------------------------------------------------------------------------------------

-- * sendTransaction

------------------------------------------------------------------------------------------------

-- | Default configuration used for sending transactions. Leaves preflight
-- checks and retry behavior at the node's own defaults; callers who want
-- fire-and-forget behavior (no preflight simulation, no rebroadcast) can use
-- 'sendTransaction'' with an explicit configuration instead.
defaultRpcSendTransactionConfig :: ConfigurationObject
defaultRpcSendTransactionConfig =
  defaultConfigObject
    { encoding = Just "base64"
    }

-- | Sends a base64-encoded transaction to the cluster using the default configuration.
sendTransaction :: (JsonRpc m) => String -> m SolanaSignature
sendTransaction tx = sendTransaction' tx defaultRpcSendTransactionConfig
{-# INLINE sendTransaction #-}

-- | Sends a base64-encoded transaction to the cluster with the specified configuration.
sendTransaction' :: (JsonRpc m) => String -> ConfigurationObject -> m SolanaSignature
sendTransaction' = do
  remote "sendTransaction"
{-# INLINE sendTransaction' #-}

------------------------------------------------------------------------------------------------

-- * simulateTransaction

------------------------------------------------------------------------------------------------

-- | Simulates a base64-encoded transaction with the given configuration.
simulateTransaction' :: (JsonRpc m) => String -> ConfigurationObject -> m Object
simulateTransaction' = do
  remote "simulateTransaction"
{-# INLINE simulateTransaction' #-}

-- | Simulates a base64-encoded transaction using a basic configuration with base64 encoding.
simulateTransaction :: (JsonRpc m) => String -> m Object
simulateTransaction tx = simulateTransaction' tx cfgJustEncodingBase64
{-# INLINE simulateTransaction #-}