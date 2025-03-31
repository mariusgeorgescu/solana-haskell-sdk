{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.Types where

import Core.Account (Lamport)
import Core.Block (BlockHash)
import Core.Crypto
import Core.Transaction (Transaction)
import Data.Aeson.Types
import Data.Int (Int64)
import Data.Map
import Data.Text
import Data.Word
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * RPCResponse

------------------------------------------------------------------------------------------------

data Context = Context
  { apiVersion :: Text,
    contextSlot :: Slot
  }
  deriving (Show, Generic)

instance FromJSON Context where
  parseJSON :: Value -> Parser Context
  parseJSON = withObject "Context" $ \v ->
    Context
      <$> v .: "apiVersion"
      <*> v .: "slot"

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

-- * BlockInfo

------------------------------------------------------------------------------------------------

data TransactionWithMeta = TransactionWithMeta
  { meta :: Maybe Object,
    -- | Transaction information.
    transaction :: Transaction
  }
  deriving (Generic, Show, Eq, FromJSON)

------------------------------------------------------------------------------------------------

-- * BlockInfo

------------------------------------------------------------------------------------------------

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
  deriving (Show, Generic)

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

------------------------------------------------------------------------------------------------

-- * BlockCommitment

------------------------------------------------------------------------------------------------

data BlockCommitment = BlockCommitment
  { -- | Array of 'Word64' logging the amount of cluster stake in lamports that has voted on the block at each depth from 0 to MAX_LOCKOUT_HISTORY
    commitmentList :: Maybe [Word64],
    -- | Total active stake, in lamports, of the current epoch.
    totalStake :: Integer
  }
  deriving (Generic, Eq, Show)

instance FromJSON BlockCommitment where
  parseJSON :: Value -> Parser BlockCommitment
  parseJSON = withObject "BlockCommitment" $ \v ->
    BlockCommitment
      <$> v .: "commitment"
      <*> v .: "totalStake"

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

------------------------------------------------------------------------------------------------

-- * HighestSnapshotSlot

------------------------------------------------------------------------------------------------

-- | Contains the highest slot information that the node has snapshots for.
data HighestSnapshotSlot = HighestSnapshotSlot
  { -- |  The highest full snapshot slot
    full :: Word64,
    -- | The highest incremental snapshot slot based on full
    incremental :: Maybe Word64
  }
  deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * NodeIdentity

------------------------------------------------------------------------------------------------

-- | Contains the identity of a node.
newtype NodeIdentity = NodeIdentity
  { identity :: SolanaPublicKey
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * InflationGovernor

------------------------------------------------------------------------------------------------

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
  deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * InflationRate

------------------------------------------------------------------------------------------------

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
  deriving (Show, Generic)

instance FromJSON InflationRate where
  parseJSON :: Value -> Parser InflationRate
  parseJSON = withObject "InflationRate" $ \v ->
    InflationRate
      <$> v .: "total"
      <*> v .: "validator"
      <*> v .: "foundation"
      <*> v .: "epoch"

------------------------------------------------------------------------------------------------

-- * InflationReward

------------------------------------------------------------------------------------------------

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
    commission :: Maybe Word8
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

-- * AddressAndLamports

------------------------------------------------------------------------------------------------

-- | Contains the address and value of an account.
data AddressAndLamports
  = AddressAndLamports
  { -- | Account address
    address :: SolanaPublicKey,
    -- | Number of lamports in the account
    lamports :: Lamport
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * AddressAndLamports

------------------------------------------------------------------------------------------------

-- | Contains the  hash as base-58 encoded string and the height of the latest block.
data LatestBlockHash
  = LatestBlockHash
  { -- | A Hash as base-58 encoded string
    blockhash :: BlockHash,
    -- | Last block height at which the blockhash will be valid
    lastValidBlockHeight :: BlockHeight
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * AddressAndLamports

------------------------------------------------------------------------------------------------

-- | 'Map' where the keys are validator identities and values are arrays of leader slot indices
-- relative to the first slot in the requested epoch.
type LeaderSchedule = Map SolanaPublicKey [Word64]

------------------------------------------------------------------------------------------------

-- * PerformanceSample

------------------------------------------------------------------------------------------------

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
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * PrioritizationFee

------------------------------------------------------------------------------------------------

data PrioritizationFee = PrioritizationFee
  { -- | Slot in which the fee was observed
    pfSlot :: Slot,
    -- | The per-compute-unit fee paid by at least one successfully landed transaction, specified in increments of micro-lamports (0.000001 lamports)
    prioritizationFee :: Word64
  }
  deriving (Generic, Show, Eq)

instance FromJSON PrioritizationFee where
  parseJSON :: Value -> Parser PrioritizationFee
  parseJSON = withObject "PrioritizationFee" $ \v ->
    PrioritizationFee
      <$> v .: "slot"
      <*> v .: "prioritizationFee"

------------------------------------------------------------------------------------------------

-- *  TransactionSignatureInformation

------------------------------------------------------------------------------------------------

data TransactionSignatureInformation = TransactionSignatureInformation
  { -- | Transaction signature
    signature :: SolanaSignature,
    -- | The slot that contains the block with the transaction
    slotTxSig :: Slot,
    -- | Error if transaction failed, 'Nothing' if transaction succeeded.
    err :: Maybe String,
    -- | Memo associated with the transaction, 'Nothing' if no memo is present
    memo :: Maybe String,
    -- | Estimated production time, as Unix timestamp (seconds since the Unix epoch) of when transaction was processed. 'Nothing' if not available.
    blockTime :: Maybe Int64,
    -- | The transaction's cluster confirmation status;
    confirmationStatus :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON TransactionSignatureInformation where
  parseJSON :: Value -> Parser TransactionSignatureInformation
  parseJSON = withObject "TransactionSignatureInformation" $ \v ->
    TransactionSignatureInformation
      <$> v .: "signature"
      <*> v .: "slot"
      <*> v .: "err"
      <*> v .: "memo"
      <*> v .: "blockTime"
      <*> v .: "confirmationStatus"

------------------------------------------------------------------------------------------------

-- *  TransactionSignatureInformation

------------------------------------------------------------------------------------------------

data TransactionSignatureStatus = TransactionSignatureStatus
  { -- | The slot the transaction was processed
    slotTxStatus :: Slot,
    -- | Number of blocks since signature confirmation, 'Nothing' if rooted, as well as finalized by a supermajority of the cluster.
    confirmationsTxStatus :: Maybe Int,
    -- | Error if transaction failed, 'Nothing' if transaction succeeded.
    errTxStatus :: Maybe String,
    -- | The transaction's cluster confirmation status.
    confirmationStatusTxStatus :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance FromJSON TransactionSignatureStatus where
  parseJSON :: Value -> Parser TransactionSignatureStatus
  parseJSON = withObject "TransactionSignatureStatus" $ \v ->
    TransactionSignatureStatus
      <$> v .: "slot"
      <*> v .: "confirmations"
      <*> v .: "err"
      <*> v .: "confirmationStatus"

newtype SearchTransactionHistory = SearchTransactionHistory
  {searchTransactionHistory :: Bool}
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------------------------------------------------------------

-- *  SolanaSupply

------------------------------------------------------------------------------------------------

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
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------------------------------------------------------------

-- *  TokenAccountBalance

------------------------------------------------------------------------------------------------

-- | Contains information about amount.
data AmmountObject = AmmountObject
  { -- | The raw balance without decimals, a string representation of u64
    amount :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals :: Word8,
    -- | The balance, using mint-prescribed decimals DEPRECATED.
    uiAmount :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString :: String
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------------------------------------------------------------

-- *  TokenAccountBalance2

------------------------------------------------------------------------------------------------

-- | Contains information about the solana token supply.
data AmmountObjectWithAddr = AmmountObjectWithAddr
  { -- | The raw balance without decimals, a string representation of u64
    address' :: SolanaPublicKey,
    -- | The raw balance without decimals, a string representation of u64
    amount' :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals' :: Word8,
    -- | The balance, using mint-prescribed decimals DEPRECATED.
    uiAmount' :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString' :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON AmmountObjectWithAddr where
  parseJSON :: Value -> Parser AmmountObjectWithAddr
  parseJSON = withObject "AmmountObjectWithAddr" $ \v ->
    AmmountObjectWithAddr
      <$> v .: "address"
      <*> v .: "amount"
      <*> v .: "decimals"
      <*> v .: "uiAmount"
      <*> v .: "uiAmountString"

------------------------------------------------------------------------------------------------

-- *  SolanaVersion

------------------------------------------------------------------------------------------------

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

-- *  TransactionResult

------------------------------------------------------------------------------------------------

-- | Contains information about the current Solana version running on the node.
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
  deriving (Generic, Show, Eq)

instance FromJSON TransactionResult where
  parseJSON :: Value -> Parser TransactionResult
  parseJSON = withObject "TransactionResult" $ \v ->
    TransactionResult
      <$> v .: "slot"
      <*> v .: "blockTime"
      <*> (v .: "meta")
      <*> v .: "transaction"

------------------------------------------------------------------------------------------------

-- *  SolanaPubKeyPurpose

------------------------------------------------------------------------------------------------

data SolanaPubKeyWithPurpose = Mint SolanaPublicKey | Program SolanaPublicKey
  deriving (Generic)

instance ToJSON SolanaPubKeyWithPurpose where
  toJSON :: SolanaPubKeyWithPurpose -> Value
  toJSON (Mint key) = object ["mint" .= key]
  toJSON (Program key) = object ["programId" .= key]

------------------------------------------------------------------------------------------------

-- *  ConfigurationObject

------------------------------------------------------------------------------------------------

data ConfigurationObject = ConfigurationObject
  { commitment :: Maybe String,
    encoding :: Maybe String,
    dataSlice :: Maybe Object,
    skipPreflight :: Maybe Bool,
    preflightCommitment :: Maybe String,
    maxRetries :: Maybe Int,
    minContextSlot :: Maybe Int
  }
  deriving (Generic, Show, ToJSON)

defaultConfigObject :: ConfigurationObject
defaultConfigObject =
  ConfigurationObject
    { commitment = Nothing,
      encoding = Nothing,
      dataSlice = Nothing,
      skipPreflight = Nothing,
      preflightCommitment = Nothing,
      maxRetries = Nothing,
      minContextSlot = Nothing
    }

cfgJustEncodingBase64 :: ConfigurationObject
cfgJustEncodingBase64 =
  defaultConfigObject
    { encoding = Just "base64"
    }
