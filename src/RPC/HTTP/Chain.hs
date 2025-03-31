{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Chain where

import Core.Account (Lamport)
import Core.Block
import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Aeson.Types
import Data.Map
import Data.Word
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getClusterNodes

------------------------------------------------------------------------------------------------

-- | Returns information about all the nodes participating in the cluster 'ClusterNodes'.
getClusterNodes :: (JsonRpc m) => m [ClusterNodes]
getClusterNodes = do
  remote "getClusterNodes"
{-# INLINE getClusterNodes #-}

-- | Contains information about all the nodes participating in the cluster
data ClusterNodes = ClusterNodes
  { pubkey :: SolanaPublicKey, --- ^  Node public key
    gossip :: Maybe String, --- ^ Gossip network address for the node
    tpu :: Maybe String, --- ^ TPU network address for the node
    rpc :: Maybe String, --- ^ JSON RPC network address for the node, or 'Nothing' if the JSON RPC service is not enabled
    version :: Maybe String, --- ^ The software version of the node, or 'Nothing' if the version information is not available
    featureSet :: Maybe Word32, --- ^ The unique identifier of the node's feature set
    sharedVersion :: Maybe Word16 ---  ^ The shred version the node has been configured to use
  }
  deriving (Show, Generic, FromJSON)

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

-- | Returns the epoch schedule information from this cluster's genesis config.
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

-- * getHealth

------------------------------------------------------------------------------------------------

-- | Returns the current health of the node.
-- A healthy node is one that is within HEALTH_CHECK_SLOT_DISTANCE slots of the latest cluster confirmed slot.
getHealth :: (JsonRpc m) => m String
getHealth = do
  remote "getHealth"
{-# INLINE getHealth #-}

------------------------------------------------------------------------------------------------

-- * getHighestSnapshotSlot

------------------------------------------------------------------------------------------------

-- | Returns the highest slot information that the node has snapshots for.
-- This will find the highest full snapshot slot, and the highest incremental snapshot slot based on the full snapshot slot, if there is one.
getHighestSnapshotSlot :: (JsonRpc m) => m HighestSnapshotSlot
getHighestSnapshotSlot = do
  remote "getHighestSnapshotSlot"
{-# INLINE getHighestSnapshotSlot #-}

-- | Contains the highest slot information that the node has snapshots for.
data HighestSnapshotSlot = HighestSnapshotSlot
  { -- |  The highest full snapshot slot
    full :: Word64,
    -- | The highest incremental snapshot slot based on full
    incremental :: Maybe Word64
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getIdentity

------------------------------------------------------------------------------------------------

-- | Returns the 'NodeIdentity'for the current node.
getIdentity' :: (JsonRpc m) => m NodeIdentity
getIdentity' = do
  remote "getIdentity"
{-# INLINE getIdentity' #-}

-- | Returns the identity pubkey for the current node.
getIdentity :: (JsonRpc m) => m SolanaPublicKey
getIdentity = identity <$> getIdentity'
{-# INLINE getIdentity #-}

-- | Contains the identity of a node.
newtype NodeIdentity = NodeIdentity
  { identity :: SolanaPublicKey
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getInflationGovernor

------------------------------------------------------------------------------------------------

-- | Returns the current inflation governor.
getInflationGovernor :: (JsonRpc m) => m InflationGovernor
getInflationGovernor = do
  remote "getInflationGovernor"
{-# INLINE getInflationGovernor #-}

-- | Contains the identity of a node.
data InflationGovernor = InflationGovernor
  { -- | Percentage of total inflation allocated to the foundation
    foundation :: Double,
    -- | Duration of foundation pool inflation in years
    foundationTerm :: Double,
    -- | Initial inflation percentage from time 0
    initial :: Double,
    -- | Rate per year at which inflation is lowered. (Rate reduction is derived using the target slot time in genesis config)
    taper :: Double,
    -- |  Terminal inflation percentage
    terminal :: Double
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getInflationRate

------------------------------------------------------------------------------------------------

-- | Returns the specific inflation values for the current epoch.
getInflationRate :: (JsonRpc m) => m InflationRate
getInflationRate = do
  remote "getInflationRate"
{-# INLINE getInflationRate #-}

-- | Contains the inflation values for the given epoch.
data InflationRate = InflationRate
  { -- | Total inflation.
    totalInflation :: Double,
    -- | Inflation allocated to validators.
    validatorInflation :: Double,
    -- | Inflation allocated to the foundation.
    foundationInflation :: Double,
    -- | Epoch for which these values are valid.
    epochInflation :: Double
  }
  deriving (Show)

instance FromJSON InflationRate where
  parseJSON :: Value -> Parser InflationRate
  parseJSON = withObject "InflationRate" $ \v ->
    InflationRate
      <$> v .: "total"
      <*> v .: "validator"
      <*> v .: "foundation"
      <*> v .: "epoch"

------------------------------------------------------------------------------------------------

-- * getInflationReward

------------------------------------------------------------------------------------------------

-- | Returns the inflation / staking reward for a list of addresses for an epoch.
getInflationReward :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe InflationReward]
getInflationReward = do
  remote "getInflationReward"
{-# INLINE getInflationReward #-}

-- | Contains the inflation / staking reward for a list of addresses for an epoch
data InflationReward = InflationReward
  { -- | Epoch for which reward occurred.
    epochReward :: Word64,
    -- | The slot in which the rewards are effective.
    effectiveSlot :: Slot,
    -- | Reward amount in lamports.
    amountReward :: Lamport,
    -- | Post balance of the account in lamports.
    postBalance :: Lamport,
    -- | Vote account commission when the reward was credited.
    commissionIR :: Maybe Word8
  }
  deriving (Show, Generic)

instance FromJSON InflationReward where
  parseJSON :: Value -> Parser InflationReward
  parseJSON = withObject "InflationReward" $ \v ->
    InflationReward
      <$> v .: "epoch"
      <*> v .: "effectiveSlot"
      <*> v .: "amount"
      <*> v .: "postBalance"
      <*> v .: "commission"

------------------------------------------------------------------------------------------------

-- * getLeaderSchedule

------------------------------------------------------------------------------------------------

-- | Get the leader schedule for an epoch.
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

-- * getMinimumBalanceForRentExemption

------------------------------------------------------------------------------------------------

-- | Get the minimum balance required to make account rent exempt.
--  Receives the Account's data length and returns the balance balance in 'Lamport'.
getMinimumBalanceForRentExemption :: (JsonRpc m) => Int -> m Lamport
getMinimumBalanceForRentExemption = do
  remote "getMinimumBalanceForRentExemption"
{-# INLINE getMinimumBalanceForRentExemption #-}

------------------------------------------------------------------------------------------------

-- * getRecentPerformanceSamples

------------------------------------------------------------------------------------------------

-- | Returns a list of recent 'PerformanceSample', in reverse slot order.
-- Performance samples are taken every 60 seconds
-- and include the number of transactions and slots that occur in a given time window.
-- Takes as optional parameter the number of samples to return (throws exception if is > 720).
getRecentPerformanceSamples :: (JsonRpc m) => Maybe Int -> m [PerformanceSample]
getRecentPerformanceSamples = do
  remote "getRecentPerformanceSamples"
{-# INLINE getRecentPerformanceSamples #-}

data PerformanceSample = PerformanceSample
  { -- | Slot in which sample was taken at
    slot :: Slot,
    -- | Number of transactions processed during the sample period
    numTransactions :: Word64,
    -- | Number of slots completed during the sample period
    numSlots :: Word64,
    -- | Number of seconds in a sample window
    samplePeriodSecs :: Word16,
    -- | Number of non-vote transactions processed during the sample period
    numNonVoteTransactions :: Word64
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- * getRecentPrioritizationFees

------------------------------------------------------------------------------------------------

-- | Returns a list of prioritization fees from recent blocks.
-- Takes and optional list of account addresses (up to a maximum of 128 addresses) @[SolanaPublicKey]@ .
-- If this parameter is provided, the response will reflect a fee to land a transaction locking all of the provided accounts as writable.
getRecentPrioritizationFees :: (JsonRpc m) => Maybe [SolanaPublicKey] -> m [PrioritizationFee]
getRecentPrioritizationFees = do
  remote "getRecentPrioritizationFees"
{-# INLINE getRecentPrioritizationFees #-}

data PrioritizationFee = PrioritizationFee
  { -- | Slot in which the fee was observed
    pfSlot :: Slot,
    -- | The per-compute-unit fee paid by at least one successfully landed transaction, specified in increments of micro-lamports (0.000001 lamports)
    prioritizationFee :: Word64
  }
  deriving (Generic, Show)

instance FromJSON PrioritizationFee where
  parseJSON :: Value -> Parser PrioritizationFee
  parseJSON = withObject "PrioritizationFee" $ \v ->
    PrioritizationFee
      <$> v .: "slot"
      <*> v .: "prioritizationFee"

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

-- * getStakeMinimumDelegation

------------------------------------------------------------------------------------------------

-- | Returns the stake minimum delegation, in lamports.
-- Returns @RpcResponse Lamport@ with value field set to @Lamport@.
getStakeMinimumDelegation' :: (JsonRpc m) => m (RPCResponse Lamport)
getStakeMinimumDelegation' = do
  remote "getStakeMinimumDelegation"

-- | Returns the stake minimum delegation, in lamports.
getStakeMinimumDelegation :: (JsonRpc m) => m Lamport
getStakeMinimumDelegation = value <$> getStakeMinimumDelegation'

------------------------------------------------------------------------------------------------

-- * getStakeMinimgetSupplyumDelegation

------------------------------------------------------------------------------------------------

-- | Returns information about the current supply.
-- Returns @RpcResponse SolanaSupply@ with value field set to @SolanaSupply@.
getSupply' :: (JsonRpc m) => m (RPCResponse SolanaSupply)
getSupply' = do
  remote "getSupply"

-- | Returns information about the current supply.
-- Returns @RpcResponse SolanaSupply@ with value field set to @SolanaSupply@.
getSupply :: (JsonRpc m) => m SolanaSupply
getSupply = value <$> getSupply'

-- | Contains information about the solana token supply.
data SolanaSupply = SolanaSupply
  { -- | Total supply in lamports
    total :: Lamport,
    -- | Circulating supply in lamports.
    circulating :: Lamport,
    -- | Non-circulating supply in lamports.
    nonCirculating :: Lamport,
    -- | An array of account addresses of non-circulating accounts.
    nonCirculatingAccounts :: [SolanaPublicKey]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getTransactionCount

------------------------------------------------------------------------------------------------

-- | Returns the current Transaction count from the ledger
getTransactionCount :: (JsonRpc m) => m Slot
getTransactionCount = do
  remote "getTransactionCount"

------------------------------------------------------------------------------------------------

-- * getVersion

------------------------------------------------------------------------------------------------

-- | Returns the current Solana version running on the node.
getVersion :: (JsonRpc m) => m SolanaVersion
getVersion = do
  remote "getVersion"

-- | Contains information about the current Solana version running on the node.
data SolanaVersion = SolanaVersion
  { -- | Software version of solana-core.
    solana_core :: String,
    -- | Unique identifier of the current software's feature set
    feature_set :: Word32
  }
  deriving (Generic, Show, Eq)

instance FromJSON SolanaVersion where
  parseJSON :: Value -> Parser SolanaVersion
  parseJSON = withObject "SolanaVersion" $ \v ->
    SolanaVersion
      <$> v .: "solana-core"
      <*> v .: "feature-set"

------------------------------------------------------------------------------------------------

-- * getVoteAccounts

------------------------------------------------------------------------------------------------

-- | Returns the account info and associated stake for all the voting accounts in the current bank.
getVoteAccounts :: (JsonRpc m) => m VoteAccounts
getVoteAccounts = do
  remote "getVoteAccounts"

data VoteAccounts = VoteAccounts
  { current :: [VoteAccountsResult],
    delinquent :: [VoteAccountsResult]
  }
  deriving (Generic, Show, FromJSON)

data VoteAccountsResult = VoteAccountsResult
  { -- | Vote account address
    votePubkey :: SolanaPublicKey,
    -- | Validator identity
    nodePubkey :: SolanaPublicKey,
    -- | The stake, in lamports, delegated to this vote account and active in this epoch
    activatedStake :: Word64,
    -- | Whether the vote account is staked for this epoch
    epochVoteAccount :: Bool,
    -- | Percentage (0-100) of rewards payout owed to the vote account
    commission :: Int,
    -- | Most recent slot voted on by this vote account
    lastVote :: Slot,
    -- | Latest history of earned credits for up to five epochs, as an array of arrays containing: [epoch, credits, previousCredits]
    epochCredits :: [[Word64]],
    -- | Current root slot for this vote account
    rootSlot :: Slot
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- * minimumLedgerSlot

------------------------------------------------------------------------------------------------

-- | Returns the lowest slot that the node has information about in its ledger.
minimumLedgerSlot :: (JsonRpc m) => m Slot
minimumLedgerSlot = do
  remote "minimumLedgerSlot"
