{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.RPC.HTTP.Chain where

import Data.Aeson
import Data.Aeson.Types
import Data.Word
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getClusterNodes

------------------------------------------------------------------------------------------------

-- | Returns information about all the nodes currently participating in the cluster.
getClusterNodes :: (JsonRpc m) => m [ClusterNodes]
getClusterNodes = do
  remote "getClusterNodes"
{-# INLINE getClusterNodes #-}

-- | Contains information about all the nodes participating in the cluster.
data ClusterNodes = ClusterNodes
  { -- | The node's public key.
    pubkey :: SolanaPublicKey,
    -- | Gossip network address for the node.
    gossip :: Maybe String,
    -- | TPU (Transaction Processing Unit) network address.
    tpu :: Maybe String,
    -- | JSON-RPC address, if the RPC service is enabled.
    rpc :: Maybe String,
    -- | Software version of the node, if available.
    version :: Maybe String,
    -- | Unique identifier of the node's feature set.
    featureSet :: Maybe Word32,
    -- | Shred version used by the node.
    sharedVersion :: Maybe Word16
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getHealth

------------------------------------------------------------------------------------------------

-- | Checks if the node is healthy.
--
-- A healthy node is within a certain number of slots (defined by 'HEALTH_CHECK_SLOT_DISTANCE')
-- of the latest confirmed cluster slot. Returns @"ok"@ if healthy; otherwise, an error is returned.
getHealth :: (JsonRpc m) => m String
getHealth = do
  remote "getHealth"
{-# INLINE getHealth #-}

------------------------------------------------------------------------------------------------

-- * getHighestSnapshotSlot

------------------------------------------------------------------------------------------------

-- | Returns the highest full and incremental snapshot slots that the node has generated.
--
-- This includes the highest full snapshot slot and, if available, the corresponding highest incremental snapshot slot.
getHighestSnapshotSlot :: (JsonRpc m) => m HighestSnapshotSlot
getHighestSnapshotSlot = do
  remote "getHighestSnapshotSlot"
{-# INLINE getHighestSnapshotSlot #-}

-- | Contains the highest slot information that the node has snapshots for.
data HighestSnapshotSlot = HighestSnapshotSlot
  { -- | The highest full snapshot slot.
    full :: Word64,
    -- | The highest incremental snapshot slot, if available.
    incremental :: Maybe Word64
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getIdentity

------------------------------------------------------------------------------------------------

-- | Returns the full 'NodeIdentity' object for the current node.
getIdentity' :: (JsonRpc m) => m NodeIdentity
getIdentity' = do
  remote "getIdentity"
{-# INLINE getIdentity' #-}

-- | Returns the identity public key of the current node.
getIdentity :: (JsonRpc m) => m SolanaPublicKey
getIdentity = identity <$> getIdentity'
{-# INLINE getIdentity #-}

-- | Identity information for the current node.
newtype NodeIdentity = NodeIdentity
  { -- | The node's identity public key.
    identity :: SolanaPublicKey
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getRecentPerformanceSamples

------------------------------------------------------------------------------------------------

-- | Returns a list of recent 'PerformanceSample' entries, in reverse slot order.
--
-- Performance samples are collected every 60 seconds and include metrics such as the number
-- of transactions, slots, and non-vote transactions within each sample window.
-- Optionally takes the number of samples to return (must be ≤ 720).
getRecentPerformanceSamples :: (JsonRpc m) => Maybe Int -> m [PerformanceSample]
getRecentPerformanceSamples = do
  remote "getRecentPerformanceSamples"
{-# INLINE getRecentPerformanceSamples #-}

-- | Performance metrics sampled periodically from the node.
data PerformanceSample = PerformanceSample
  { -- | Slot in which the sample was taken.
    slot :: Slot,
    -- | Number of transactions during the sample period.
    numTransactions :: Word64,
    -- | Number of slots completed during the sample.
    numSlots :: Word64,
    -- | Duration of the sampling window, in seconds.
    samplePeriodSecs :: Word16,
    -- | Number of non-vote transactions during the sample.
    numNonVoteTransactions :: Word64
  }
  deriving (Generic, Show, FromJSON)

------------------------------------------------------------------------------------------------

-- * getRecentPrioritizationFees

------------------------------------------------------------------------------------------------

-- | Returns recent prioritization fees observed in recent blocks.
--
-- Optionally takes a list of up to 128 account addresses. If provided, the response estimates
-- the prioritization fee for landing a transaction that locks all listed accounts as writable.
getRecentPrioritizationFees :: (JsonRpc m) => Maybe [SolanaPublicKey] -> m [PrioritizationFee]
getRecentPrioritizationFees = do
  remote "getRecentPrioritizationFees"
{-# INLINE getRecentPrioritizationFees #-}

-- | A prioritization fee sample from a recent block.
data PrioritizationFee = PrioritizationFee
  { -- | Slot in which the fee was observed.
    pfSlot :: Slot,
    -- | Per-compute-unit fee (in micro-lamports).
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

-- | Returns the Solana software version and feature set currently running on the node.
getVersion :: (JsonRpc m) => m SolanaVersion
getVersion = do
  remote "getVersion"

-- | Contains information about the software version and feature set identifier of the node.
data SolanaVersion = SolanaVersion
  { -- | Software version of 'solana-core'.
    solana_core :: String,
    -- | Unique identifier of the software's feature set.
    feature_set :: Word32
  }
  deriving (Generic, Show, Eq)

instance FromJSON SolanaVersion where
  parseJSON :: Value -> Parser SolanaVersion
  parseJSON = withObject "SolanaVersion" $ \v ->
    SolanaVersion
      <$> v .: "solana-core"
      <*> v .: "feature-set"
