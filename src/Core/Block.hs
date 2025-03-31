{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Block where

import Core.Crypto (fromBase58String, toBase58String)
import Data.Aeson.Types
import Data.Binary
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.Maybe (fromJust)
import Data.Text qualified as Text
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
  show (BlockHash bs) = toBase58String bs

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
  parseJSON = withText "BlockHash" $ return . unsafeBlockHash . Text.unpack

unsafeBlockHash :: String -> BlockHash
unsafeBlockHash = BlockHash . fromJust . fromBase58String
