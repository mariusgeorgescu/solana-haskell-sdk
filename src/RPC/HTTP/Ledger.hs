{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Ledger where

import Core.Block
import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Map
import Data.Word
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getEpochInfo

------------------------------------------------------------------------------------------------

-- | Returns information about the current epoch.
getEpochInfo :: (JsonRpc m) => m EpochInfo
getEpochInfo = do
  remote "getEpochInfo"
{-# INLINE getEpochInfo #-}

-- | Solana Epoch
type Epoch = Word64

-- | Contains information about a Solana Epoch
data EpochInfo = EpochInfo
  { absoluteSlot :: Slot, --- ^  The current slot
    blockHeight :: Word64, --- ^ The current epoch
    epoch :: Epoch, --- ^ The current epoch
    slotIndex :: Word64, --- ^ The current slot relative to the start of the current epoch
    slotsInEpoch :: Word64, --- ^ The number of slots in this epoch
    transactionCount :: Maybe Word64 --- ^ Total number of transactions processed without error since genesis
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getEpochSchedule

------------------------------------------------------------------------------------------------

-- | Returns the epoch scheduling parameters configured in the cluster's genesis..
getEpochSchedule :: (JsonRpc m) => m EpochSchedule
getEpochSchedule = do
  remote "getEpochSchedule"
{-# INLINE getEpochSchedule #-}

-- | Contains epoch schedule information from this cluster's genesis config
data EpochSchedule = EpochSchedule
  { -- |  The maximum number of slots in each epoch.
    slotsPerEpoch :: Word64,
    -- | The number of slots before beginning of an epoch to calculate a leader schedule for that epoch.
    leaderScheduleSlotOffset :: Word64,
    -- | Whether epochs start short and grow
    warmup :: Bool,
    -- | First normal-length epoch, log2(slotsPerEpoch) - log2(MINIMUM_SLOTS_PER_EPOCH)
    firstNormalEpoch :: Word64,
    -- | Minimum number of slots in an epoch, MINIMUM_SLOTS_PER_EPOCH * (2.pow(firstNormalEpoch) - 1)
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

-- | Returns the genesis hash.
getGenesisHash :: (JsonRpc m) => m BlockHash
getGenesisHash = do
  remote "getGenesisHash"
{-# INLINE getGenesisHash #-}

------------------------------------------------------------------------------------------------

-- * getLeaderSchedule

------------------------------------------------------------------------------------------------

-- | Returns the leader schedule mapping validator identities to their leader slots.
-- Fetch the leader schedule for the epoch that corresponds to the provided slot.
-- If unspecified, the leader schedule for the current epoch is fetched.
getLeaderSchedule :: (JsonRpc m) => Maybe Slot -> m (Maybe LeaderSchedule)
getLeaderSchedule = do
  remote "getLeaderSchedule"
{-# INLINE getLeaderSchedule #-}

-- | 'Map' where the keys are validator identities and values are arrays of leader slot indices
-- relative to the first slot in the requested epoch.
type LeaderSchedule = Map SolanaPublicKey [Word64]

------------------------------------------------------------------------------------------------

-- * getMaxRetransmitSlot

------------------------------------------------------------------------------------------------

-- | Get the max slot seen from retransmit stage.
getMaxRetransmitSlot :: (JsonRpc m) => m Slot
getMaxRetransmitSlot = do
  remote "getMaxRetransmitSlot"
{-# INLINE getMaxRetransmitSlot #-}

------------------------------------------------------------------------------------------------

-- * getMaxShredInsertSlot

------------------------------------------------------------------------------------------------

-- | Get the max slot seen from after shred insert.
getMaxShredInsertSlot :: (JsonRpc m) => m Slot
getMaxShredInsertSlot = do
  remote "getMaxShredInsertSlot"
{-# INLINE getMaxShredInsertSlot #-}

------------------------------------------------------------------------------------------------

-- * getSlot

------------------------------------------------------------------------------------------------

-- | Returns the slot that has reached the given or default commitment level
getSlot :: (JsonRpc m) => m Slot
getSlot = do
  remote "getSlot"

------------------------------------------------------------------------------------------------

-- * getSlotLeader

------------------------------------------------------------------------------------------------

-- | Returns the current slot leader
getSlotLeader :: (JsonRpc m) => m SolanaPublicKey
getSlotLeader = do
  remote "getSlotLeader"

------------------------------------------------------------------------------------------------

-- * getSlotLeaders

------------------------------------------------------------------------------------------------

-- | Returns the slot leaders for a given slot range
-- Receives the start 'Slot' and a limit as an 'Int' (between 1 and 5,000).
getSlotLeaders :: (JsonRpc m) => Slot -> Int -> m [SolanaPublicKey]
getSlotLeaders = do
  remote "getSlotLeaders"

------------------------------------------------------------------------------------------------

-- * getTransactionCount

------------------------------------------------------------------------------------------------

-- | Returns the current Transaction count from the ledger
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
