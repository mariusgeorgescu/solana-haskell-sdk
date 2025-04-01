{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Ledger
-- Description : Ledger-level Solana RPC methods for querying epoch and slot data.
--
-- This module provides bindings to Solana JSON-RPC methods related to the ledger,
-- including epoch and slot information, block leader schedules, genesis data,
-- and transaction counts. It is useful for building explorers, monitoring tools,
-- or protocols that depend on precise ledger state and validator schedule tracking.
module Network.Solana.RPC.HTTP.Ledger where

import Data.Aeson
import Data.Map
import Data.Word
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Block
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getEpochInfo

------------------------------------------------------------------------------------------------

-- | Returns information about the current epoch.
--
-- This includes the current slot, block height, epoch index, and transaction count since genesis.
getEpochInfo :: (JsonRpc m) => m EpochInfo
getEpochInfo = do
  remote "getEpochInfo"
{-# INLINE getEpochInfo #-}

-- | Solana epoch index.
type Epoch = Word64

-- | Contains information about the current epoch.
data EpochInfo = EpochInfo
  { -- | The current absolute slot.
    absoluteSlot :: Slot,
    -- | The current block height.
    blockHeight :: Word64,
    -- | The current epoch index.
    epoch :: Epoch,
    -- | Slot index relative to the start of the epoch.
    slotIndex :: Word64,
    -- | Total number of slots in the epoch.
    slotsInEpoch :: Word64,
    -- | Total number of successful transactions since genesis.
    transactionCount :: Maybe Word64
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getEpochSchedule

------------------------------------------------------------------------------------------------

-- | Returns the epoch scheduling parameters configured in the cluster's genesis configuration.
getEpochSchedule :: (JsonRpc m) => m EpochSchedule
getEpochSchedule = do
  remote "getEpochSchedule"
{-# INLINE getEpochSchedule #-}

-- | Contains epoch schedule information as defined in the cluster's genesis config.
data EpochSchedule = EpochSchedule
  { -- | Maximum number of slots per epoch.
    slotsPerEpoch :: Word64,
    -- | Number of slots before epoch start to compute leader schedule.
    leaderScheduleSlotOffset :: Word64,
    -- | Whether epochs start small and grow (warmup).
    warmup :: Bool,
    -- | First normal-length epoch.(log2(slotsPerEpoch) - log2(MINIMUM_SLOTS_PER_EPOCH))
    firstNormalEpoch :: Word64,
    -- | First slot of the first normal-length epoch. ( MINIMUM_SLOTS_PER_EPOCH * (2.pow(firstNormalEpoch) - 1))
    firstNormalSlot :: Word64
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getFirstAvailableBlock

------------------------------------------------------------------------------------------------

-- | Returns the slot of the lowest confirmed block that has not been purged from the ledger.
getFirstAvailableBlock :: (JsonRpc m) => m Slot
getFirstAvailableBlock = do
  remote "getFirstAvailableBlock"
{-# INLINE getFirstAvailableBlock #-}

------------------------------------------------------------------------------------------------

-- * getGenesisHash

------------------------------------------------------------------------------------------------

-- | Returns the block hash of the genesis block.
getGenesisHash :: (JsonRpc m) => m BlockHash
getGenesisHash = do
  remote "getGenesisHash"
{-# INLINE getGenesisHash #-}

------------------------------------------------------------------------------------------------

-- * getLeaderSchedule

------------------------------------------------------------------------------------------------

-- | Returns the leader schedule mapping validator identities to their leader slots.
--
-- Fetches the leader schedule for the epoch that contains the specified slot.
-- If no slot is specified, the schedule for the current epoch is returned.
getLeaderSchedule :: (JsonRpc m) => Maybe Slot -> m (Maybe LeaderSchedule)
getLeaderSchedule = do
  remote "getLeaderSchedule"
{-# INLINE getLeaderSchedule #-}

-- | Map from validator identity pubkeys to arrays of slot indices
-- indicating when each validator is expected to produce a block.
type LeaderSchedule = Map SolanaPublicKey [Word64]

------------------------------------------------------------------------------------------------

-- * getMaxRetransmitSlot

------------------------------------------------------------------------------------------------

-- | Returns the highest slot observed by the node from the retransmit stage.
getMaxRetransmitSlot :: (JsonRpc m) => m Slot
getMaxRetransmitSlot = do
  remote "getMaxRetransmitSlot"
{-# INLINE getMaxRetransmitSlot #-}

------------------------------------------------------------------------------------------------

-- * getMaxShredInsertSlot

------------------------------------------------------------------------------------------------

-- | Returns the highest slot observed by the node after shred insertion.
getMaxShredInsertSlot :: (JsonRpc m) => m Slot
getMaxShredInsertSlot = do
  remote "getMaxShredInsertSlot"
{-# INLINE getMaxShredInsertSlot #-}

------------------------------------------------------------------------------------------------

-- * getSlot

------------------------------------------------------------------------------------------------

-- | Returns the current slot, based on the default or specified commitment level.
getSlot :: (JsonRpc m) => m Slot
getSlot = do
  remote "getSlot"

------------------------------------------------------------------------------------------------

-- * getSlotLeader

------------------------------------------------------------------------------------------------

-- | Returns the identity public key of the current slot leader.
getSlotLeader :: (JsonRpc m) => m SolanaPublicKey
getSlotLeader = do
  remote "getSlotLeader"

------------------------------------------------------------------------------------------------

-- * getSlotLeaders

------------------------------------------------------------------------------------------------

-- | Returns the list of slot leaders for a given slot range.
--
-- Takes a starting 'Slot' and a limit (between 1 and 5,000), and returns the validator identities
-- that will produce blocks for the given slots.
getSlotLeaders :: (JsonRpc m) => Slot -> Int -> m [SolanaPublicKey]
getSlotLeaders = do
  remote "getSlotLeaders"

------------------------------------------------------------------------------------------------

-- * getTransactionCount

------------------------------------------------------------------------------------------------

-- | Returns the total number of transactions recorded in the ledger.
getTransactionCount :: (JsonRpc m) => m Slot
getTransactionCount = do
  remote "getTransactionCount"

------------------------------------------------------------------------------------------------

-- * minimumLedgerSlot

------------------------------------------------------------------------------------------------

-- | Returns the lowest slot that the node has information about in its ledger.
minimumLedgerSlot :: (JsonRpc m) => m Slot
minimumLedgerSlot = do
  remote "minimumLedgerSlot"
