{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Block where

import Core.Crypto
import Data.Aeson.Types
import Data.Binary
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.ByteString.Base58
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Text.Encoding
import GHC.Generics (Generic)

------------------------------------------------------------------------------------------------

-- * BlockHash

------------------------------------------------------------------------------------------------

newtype BlockHash = BlockHash S.ByteString
  deriving (Eq, Generic)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)

instance Show BlockHash where
  show :: BlockHash -> String
  show (BlockHash bs) = show $ encodeBase58 bitcoinAlphabet bs

instance Binary BlockHash where
  put :: BlockHash -> Put
  put (BlockHash bs) = putByteString bs -- not default Binary instance (wo length)
  get :: Get BlockHash
  get = BlockHash <$> get

instance ToJSON BlockHash where
  toJSON :: BlockHash -> Value
  toJSON bh = toJSON (show bh)

instance FromJSON BlockHash where
  parseJSON :: Value -> Parser BlockHash
  parseJSON = withText "BlockHash" $ return . (unsafeBlockHash . encodeUtf8)

unsafeBlockHash :: S.ByteString -> BlockHash
unsafeBlockHash = BlockHash . fromJust . decodeBase58 bitcoinAlphabet

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

type BlockHeight = Int

type Slot = Int

------------------------------------------------------------------------------------------------

-- * BlockProduction

------------------------------------------------------------------------------------------------

-- | Two element array containing the number of leader slots and the number of blocks produced.
type ValidatorData = [Int]

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
