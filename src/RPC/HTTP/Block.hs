{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Block where

import Core.Block
import Core.Crypto (SolanaPublicKey)
import Core.Transaction (Transaction)
import Data.Aeson
import Data.Aeson.Types
import Data.Int
import Data.Map
import Data.Word (Word64)
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * RPC Methods

------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- ** getBlock

------------------------------------------------------------------------------------------------

-- | Returns identity and transaction information about a confirmed block in the ledger.
getBlock :: (JsonRpc m) => Slot -> m BlockInfo
getBlock = do
  remote "getBlock"
{-# INLINE getBlock #-}

data BlockInfo = BlockInfo
  { -- | The blockhash of this block,
    blockhashBI :: BlockHash,
    -- | The blockhash of this block's parent, if the parent block is not available due to ledger cleanup, this field will return "11111111111111111111111111111111".
    previousBlockhashBI :: BlockHash,
    -- | The slot index of this block's parent
    parentSlotBI :: Slot,
    transactionsBI :: [TransactionWithMeta],
    -- | Estimated production time, as Unix timestamp (seconds since the Unix epoch). 'Nothing' if not available.
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

data TransactionWithMeta = TransactionWithMeta
  { meta :: Maybe Object,
    -- | Transaction information.
    transaction :: Transaction
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- ** getBlockCommitment

------------------------------------------------------------------------------------------------

-- | Get the block commitment based on the block number 'Solt'.
-- Returns 'BlockCommitment' for particular block
getBlockCommitment :: (JsonRpc m) => Slot -> m BlockCommitment
getBlockCommitment = do
  remote "getBlockCommitment"
{-# INLINE getBlockCommitment #-}

data BlockCommitment = BlockCommitment
  { -- | Array of 'Word64' logging the amount of cluster stake in lamports that has voted on the block at each depth from 0 to MAX_LOCKOUT_HISTORY
    commitmentList :: Maybe [Word64],
    -- | Total active stake, in lamports, of the current epoch.
    totalStake :: Integer
  }
  deriving (Show)

instance FromJSON BlockCommitment where
  parseJSON :: Value -> Parser BlockCommitment
  parseJSON = withObject "BlockCommitment" $ \v ->
    BlockCommitment
      <$> v .: "commitment"
      <*> v .: "totalStake"

------------------------------------------------------------------------------------------------

-- ** getBlockHeight

------------------------------------------------------------------------------------------------

-- | Returns the current block height of the node
getBlockHeight :: (JsonRpc m) => m BlockHeight
getBlockHeight = do
  remote "getBlockHeight"
{-# INLINE getBlockHeight #-}

------------------------------------------------------------------------------------------------

-- ** getBlockProduction

------------------------------------------------------------------------------------------------

-- | Returns recent block production information from the current or previous epoch.
-- Returns 'RpcResponse BlockProduction' with value field set to the 'BlockProduction'.
getBlockProduction' :: (JsonRpc m) => m (RPCResponse BlockProduction)
getBlockProduction' = do
  remote "getBlockProduction"
{-# INLINE getBlockProduction' #-}

-- | Returns recent block production information from the current or previous epoch.
getBlockProduction :: (JsonRpc m) => m BlockProduction
getBlockProduction = value <$> getBlockProduction'
{-# INLINE getBlockProduction #-}

type ValidatorData = [Word64]

type ByIdentity = Map SolanaPublicKey ValidatorData

data BlockProductionRange = BlockProductionRange
  { firstSlot :: Slot,
    lastSlot :: Slot
  }
  deriving (Show, Generic, FromJSON)

data BlockProduction = BlockProduction ByIdentity BlockProductionRange
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

-- | Returns a list of confirmed blocks between two slots
-- An array of integers listing confirmed blocks between start_slot and either end_slot
-- if provided, or latest confirmed slot, inclusive. Max range allowed is 500,000 slots.
getBlocks :: (JsonRpc m) => Slot -> Maybe Slot -> m [Int]
getBlocks = do
  remote "getBlocks"
{-# INLINE getBlocks #-}

------------------------------------------------------------------------------------------------

-- * getBlocksWithLimit

------------------------------------------------------------------------------------------------

-- | Returns a list of confirmed blocks starting at the given slot.
-- An array of integers listing confirmed blocks starting at start_slot for up to limit blocks, inclusive.
getBlocksWithLimit :: (JsonRpc m) => Slot -> Int -> m [Int]
getBlocksWithLimit = do
  remote "getBlocksWithLimit"
{-# INLINE getBlocksWithLimit #-}

------------------------------------------------------------------------------------------------

-- * getBlockTime

------------------------------------------------------------------------------------------------

-- | Returns the estimated production time of a block (as Unix timestamp / seconds since the Unix epoch).
-- Each validator reports their UTC time to the ledger on a regular interval by intermittently adding a timestamp to a Vote for a particular block.
-- A requested block's time is calculated from the stake-weighted mean of the Vote timestamps in a set of recent blocks recorded on the ledger.
getBlockTime :: (JsonRpc m) => Slot -> m Int
getBlockTime = do
  remote "getBlockTime"
{-# INLINE getBlockTime #-}

------------------------------------------------------------------------------------------------

-- * getLatestBlockhash

------------------------------------------------------------------------------------------------

-- | Get the hash and the height of the latest block.
-- Returns 'RpcResponse LatestBlockHash' with value field set to a list of 'LatestBlockHash'.
getLatestBlockhash' :: (JsonRpc m) => m (RPCResponse LatestBlockHash)
getLatestBlockhash' = do
  remote "getLatestBlockhash"
{-# INLINE getLatestBlockhash' #-}

-- | Get the hash and the height of the latest block as 'LatestBlockHash'.
getLatestBlockhash :: (JsonRpc m) => m LatestBlockHash
getLatestBlockhash = value <$> getLatestBlockhash'
{-# INLINE getLatestBlockhash #-}

-- | Get the 'BlockHash' of the latest block.
getTheLatestBlockhash :: (JsonRpc m) => m BlockHash
getTheLatestBlockhash = blockhash <$> getLatestBlockhash
{-# INLINE getTheLatestBlockhash #-}

-- | Contains the  hash as base-58 encoded string and the height of the latest block.
data LatestBlockHash
  = LatestBlockHash
  { -- | A Hash as base-58 encoded string
    blockhash :: BlockHash,
    -- | Last block height at which the blockhash will be valid
    lastValidBlockHeight :: BlockHeight
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- * isBlockhashValid

------------------------------------------------------------------------------------------------

-- | Returns whether a blockhash is still valid or not.
isBlockhashValid' :: (JsonRpc m) => BlockHash -> m (RPCResponse Bool)
isBlockhashValid' = do
  remote "isBlockhashValid"
{-# INLINE isBlockhashValid' #-}

-- | Returns whether a blockhash is still valid or not.
isBlockhashValid :: (JsonRpc m) => BlockHash -> m Bool
isBlockhashValid = fmap value . isBlockhashValid'
{-# INLINE isBlockhashValid #-}