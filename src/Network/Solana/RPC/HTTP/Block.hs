{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Block
-- Description : Solana RPC methods for accessing block-related information.
--
-- This module provides access to block-related JSON-RPC methods in the Solana network.
-- These include fetching full block data, commitment levels, recent block production statistics,
-- block ranges, timestamps, and validity of blockhashes. Useful for exploring historical blocks,
-- validator performance, and transaction confirmation metadata.
module Network.Solana.RPC.HTTP.Block where

import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Block
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.Core.Transaction (Transaction)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- ** getBlock

------------------------------------------------------------------------------------------------

-- | Returns identity and transaction information about a confirmed block in
-- the ledger, using the given configuration.
--
-- A slot that holds no block is answered by the node with a JSON-RPC error,
-- not with @null@: @-32007@ (slot skipped), @-32001@ (pruned from the ledger)
-- or @-32004@ (not yet produced or rooted). The transport raises such errors
-- as a @JsonRpcException@ (@CallException@) that @runWeb3'@ does not catch,
-- so wrap the call in 'Control.Exception.try' to handle them. 'Nothing' is
-- returned only when the node itself answers @null@, which Agave does solely
-- for a rooted slot missing from its blockstore without being flagged skipped.
getBlock' :: (JsonRpc m) => Slot -> ConfigurationObject -> m (Maybe BlockInfo)
getBlock' = do
  remote "getBlock"
{-# INLINE getBlock' #-}

-- | Returns identity and transaction information about a confirmed block in
-- the ledger. Sets @maxSupportedTransactionVersion@ to 1, so that blocks
-- containing v0 or v1 (SIMD-0385) transactions can be decoded -- without a
-- high enough value such blocks make the node respond with error -32015,
-- and v1 transactions are now a routine part of blocks.
--
-- Versioned transactions are returned without the message parts
-- 'Network.Solana.Core.Message.CompiledMessage' does not model: a v0
-- transaction's address-lookup-table data (so instruction account indices
-- may reference addresses not present in the parsed account keys), and a v1
-- transaction's @transactionConfig@ resource requests (its priority fee,
-- compute-unit limit, loaded-accounts size limit and heap size -- which in
-- v1 replace the @ComputeBudget@ instructions earlier formats carry, so
-- unlike a legacy transaction's, they are not visible among the parsed
-- instructions either).
-- A slot holding no block is a JSON-RPC error rather than 'Nothing'; see
-- 'getBlock''.
getBlock :: (JsonRpc m) => Slot -> m (Maybe BlockInfo)
getBlock slot = getBlock' slot (defaultConfigObject {encoding = Just "json", maxSupportedTransactionVersion = Just 1})
{-# INLINE getBlock #-}

-- | Contains identity and transaction information for a block.
data BlockInfo = BlockInfo
  { -- | The blockhash of this block.
    blockhashBI :: BlockHash,
    -- | The blockhash of this block's parent. If the parent is not available due to ledger cleanup, this will return "11111111111111111111111111111111".
    previousBlockhashBI :: BlockHash,
    -- | The slot index of this block's parent.
    parentSlotBI :: Slot,
    -- | List of transactions included in the block, each with optional metadata.
    transactionsBI :: [TransactionWithMeta],
    -- | Estimated production time, as a Unix timestamp (in seconds). 'Nothing' if not available.
    blockTimeBI :: Maybe Int64,
    -- | The number of blocks beneath this block.
    blockHeightBI :: Maybe BlockHeight
  }
  deriving (Show)

instance FromJSON BlockInfo where
  parseJSON :: Value -> Parser BlockInfo
  parseJSON = withObject "BlockInfo" $ \v ->
    BlockInfo
      <$> v .: "blockhash"
      <*> v .: "previousBlockhash"
      <*> v .: "parentSlot"
      <*> v .: "transactions"
      <*> v .: "blockTime"
      <*> v .: "blockHeight"

-- | A transaction and its optional metadata as included in a block.
data TransactionWithMeta = TransactionWithMeta
  { -- | Optional metadata for the transaction.
    meta :: Maybe Object,
    -- | The transaction details.
    transaction :: Transaction
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- ** getBlockCommitment

------------------------------------------------------------------------------------------------

-- | Get the block commitment based on the given block number 'Slot'.
-- Returns a 'BlockCommitment' for the specified block.
getBlockCommitment :: (JsonRpc m) => Slot -> m BlockCommitment
getBlockCommitment = do
  remote "getBlockCommitment"
{-# INLINE getBlockCommitment #-}

-- | Commitment information for a block.
data BlockCommitment = BlockCommitment
  { -- | Array logging the amount of cluster stake in lamports that has voted on the block at each depth from 0 to MAX_LOCKOUT_HISTORY
    commitmentList :: Maybe [Word64],
    -- | Total active stake, in lamports, for the current epoch.
    totalStake :: Integer
  }
  deriving (Show)

instance FromJSON BlockCommitment where
  parseJSON = withObject "BlockCommitment" $ \v ->
    BlockCommitment
      <$> v .: "commitment"
      <*> v .: "totalStake"

------------------------------------------------------------------------------------------------

-- ** getBlockHeight

------------------------------------------------------------------------------------------------

-- | Returns the current block height of the node.
getBlockHeight :: (JsonRpc m) => m BlockHeight
getBlockHeight = do
  remote "getBlockHeight"
{-# INLINE getBlockHeight #-}

------------------------------------------------------------------------------------------------

-- ** getBlockProduction

------------------------------------------------------------------------------------------------

-- | Returns recent block production information from the current or previous epoch.
-- Returns 'RPCResponse' 'BlockProduction' with the production statistics.
getBlockProduction' :: (JsonRpc m) => m (RPCResponse BlockProduction)
getBlockProduction' = do
  remote "getBlockProduction"
{-# INLINE getBlockProduction' #-}

-- | Returns recent block production information from the current or previous epoch.
getBlockProduction :: (JsonRpc m) => m BlockProduction
getBlockProduction = value <$> getBlockProduction'
{-# INLINE getBlockProduction #-}

-- | Per-validator production counts: a two-element array of the number of leader slots and the number of blocks produced.
type ValidatorData = [Word64]

-- | Mapping from validator identity to its production counts ('ValidatorData').
type ByIdentity = Map SolanaPublicKey ValidatorData

-- | Range of slots considered for block production.
data BlockProductionRange = BlockProductionRange
  { -- | First slot in the range.
    firstSlot :: Slot,
    -- | Last slot in the range.
    lastSlot :: Slot
  }
  deriving (Show, Generic, FromJSON)

-- | Block production statistics mapped by validator identity.
data BlockProduction
  = BlockProduction
      -- | Mapping from validator identity to number of blocks produced.
      ByIdentity
      -- | Range of slots considered.
      BlockProductionRange
  deriving (Generic, Show)

instance FromJSON BlockProduction where
  parseJSON :: Value -> Parser BlockProduction
  parseJSON = withObject "BlockProduction" $ \v ->
    BlockProduction
      <$> v .: "byIdentity"
      <*> v .: "range"

------------------------------------------------------------------------------------------------

-- * getBlocks

------------------------------------------------------------------------------------------------

-- | Returns a list of confirmed blocks between two slots (inclusive).
-- The maximum range allowed is 500,000 slots.
getBlocks :: (JsonRpc m) => Slot -> Maybe Slot -> m [Slot]
getBlocks = do
  remote "getBlocks"
{-# INLINE getBlocks #-}

------------------------------------------------------------------------------------------------

-- * getBlocksWithLimit

------------------------------------------------------------------------------------------------

-- | Returns a list of confirmed blocks starting from the given slot, up to the specified limit.
getBlocksWithLimit :: (JsonRpc m) => Slot -> Int -> m [Slot]
getBlocksWithLimit = do
  remote "getBlocksWithLimit"
{-# INLINE getBlocksWithLimit #-}

------------------------------------------------------------------------------------------------

-- * getBlockTime

------------------------------------------------------------------------------------------------

-- | Returns the estimated production time of a block (as Unix timestamp).
-- This is based on stake-weighted votes and validator-reported timestamps.
-- Returns 'Nothing' if the block exists but carries no timestamp; a slot
-- holding no block is a JSON-RPC error, as for 'getBlock''.
getBlockTime :: (JsonRpc m) => Slot -> m (Maybe Int)
getBlockTime = do
  remote "getBlockTime"
{-# INLINE getBlockTime #-}

------------------------------------------------------------------------------------------------

-- * getLatestBlockhash

------------------------------------------------------------------------------------------------

-- | Get the hash and height of the latest block.
-- Returns 'RPCResponse' 'LatestBlockHash' containing the block information.
getLatestBlockhash' :: (JsonRpc m) => m (RPCResponse LatestBlockHash)
getLatestBlockhash' = do
  remote "getLatestBlockhash"
{-# INLINE getLatestBlockhash' #-}

-- | Get the hash and height of the latest block.
getLatestBlockhash :: (JsonRpc m) => m LatestBlockHash
getLatestBlockhash = value <$> getLatestBlockhash'
{-# INLINE getLatestBlockhash #-}

-- | Get only the blockhash of the latest block.
getTheLatestBlockhash :: (JsonRpc m) => m BlockHash
getTheLatestBlockhash = blockhash <$> getLatestBlockhash
{-# INLINE getTheLatestBlockhash #-}

-- | Contains the hash and the height of the latest block.
data LatestBlockHash = LatestBlockHash
  { -- | Block hash as a base-58 encoded string.
    blockhash :: BlockHash,
    -- | Last block height at which the blockhash is still considered valid.
    lastValidBlockHeight :: BlockHeight
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- * isBlockhashValid

------------------------------------------------------------------------------------------------

-- | Returns whether a blockhash is still valid.
isBlockhashValid' :: (JsonRpc m) => BlockHash -> m (RPCResponse Bool)
isBlockhashValid' = do
  remote "isBlockhashValid"
{-# INLINE isBlockhashValid' #-}

-- | Returns whether a blockhash is still valid.
isBlockhashValid :: (JsonRpc m) => BlockHash -> m Bool
isBlockhashValid = fmap value . isBlockhashValid'
{-# INLINE isBlockhashValid #-}
