{-# LANGUAGE DerivingVia #-}

module Core.Block where

import Data.Aeson (FromJSON (parseJSON), fromJSON)
import Data.Aeson.Types
import Data.Binary
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.ByteString.Base58
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding
import GHC.Generics (Generic)

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