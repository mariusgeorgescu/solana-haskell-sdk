{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.Types where

import Core.Crypto
import Data.Aeson.Types
import Data.Map
import Data.Text
import Data.Word
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * RPCResponse

------------------------------------------------------------------------------------------------

data Context = Context
  { apiVersion :: Text,
    slot :: Slot
  }
  deriving (Show, Generic)

instance FromJSON Context

data RPCResponse a = RPCResponse
  { context :: Context,
    value :: a
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (RPCResponse a) where
  parseJSON :: (FromJSON a) => Value -> Parser (RPCResponse a)
  parseJSON = withObject "Person" $ \v ->
    RPCResponse
      <$> v .: "context"
      <*> v .: "value"

------------------------------------------------------------------------------------------------

-- * BlockCommitment

------------------------------------------------------------------------------------------------

data BlockCommitment = BlockCommitment
  { -- | Array of 'Word64' logging the amount of cluster stake in lamports that has voted on the block at each depth from 0 to MAX_LOCKOUT_HISTORY
    commitment :: Maybe [Word64],
    -- | Total active stake, in lamports, of the current epoch.
    totalStake :: Integer
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

type BlockHeight = Word64

type Slot = Word64

------------------------------------------------------------------------------------------------

-- * BlockProduction

------------------------------------------------------------------------------------------------

-- | Two element array containing the number of leader slots and the number of blocks produced.
type ValidatorData = [Word64]

-- The structure of the entire JSON object
type ByIdentity = Map SolanaPublicKey ValidatorData

data BlockProductionRange = BlockProductionRange
  { firstSlot :: Slot,
    lastSlot :: Slot
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BlockProduction = BlockProduction ByIdentity BlockProductionRange
  deriving (Generic, Show)

-- Instance to parse ByIdentity from JSON
instance FromJSON BlockProduction where
  parseJSON :: Value -> Parser BlockProduction
  parseJSON = withObject "BlockProduction" $ \v ->
    BlockProduction
      <$> v .: "byIdentity"
      <*> v .: "range"

------------------------------------------------------------------------------------------------

-- * ClusterNodes

------------------------------------------------------------------------------------------------

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
  deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * EpochInfo

------------------------------------------------------------------------------------------------

-- | Solana Epoch
type Epoch = Word64

-- | Contains information about a Solana Epoch
data EpochInfo = EpochInfo
  { absoluteSlot :: Slot, --- ^  The current slot
    blockHeight :: BlockHeight, --- ^ The current epoch
    epoch :: Epoch, --- ^ The current epoch
    slotIndex :: Word64, --- ^ The current slot relative to the start of the current epoch
    slotsInEpoch :: Word64, --- ^ The number of slots in this epoch
    transactionCount :: Maybe Word64 --- ^ Total number of transactions processed without error since genesis
  }
  deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * EpochSchedule

------------------------------------------------------------------------------------------------

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
  deriving (Show, Generic, ToJSON, FromJSON)