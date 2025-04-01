{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Chain where

import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Aeson.Types
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

-- | Returns the highest full and incremental snapshot slots the node has generated.
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

-- | Identity information for node.
newtype NodeIdentity = NodeIdentity
  { identity :: SolanaPublicKey
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

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

-- * getVersion

------------------------------------------------------------------------------------------------

-- | Returns the current Solana version running on the node.
getVersion :: (JsonRpc m) => m SolanaVersion
getVersion = do
  remote "getVersion"

-- | Contains information about Software version and feature set identifier for the running on the node.
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
