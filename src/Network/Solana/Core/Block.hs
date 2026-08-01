{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.Core.Block
-- Description : Block hashes and block heights.
module Network.Solana.Core.Block where

import Network.Solana.Core.Crypto (fromBase58String, toBase58String)
import Data.Aeson.Types
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import GHC.Generics (Generic)

------------------------------------------------------------------------------------------------

-- * BlockHash

------------------------------------------------------------------------------------------------

-- | A 32-byte hash identifying a ledger entry (block), rendered in Base58 by
-- 'Show' and the JSON instances. Transactions embed a recent block hash as
-- a proof of recency.
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
  get = BlockHash <$> getByteString 32

instance ToJSON BlockHash where
  toJSON :: BlockHash -> Value
  toJSON bh = toJSON (show bh)

instance FromJSON BlockHash where
  parseJSON :: Value -> Parser BlockHash
  parseJSON = withText "BlockHash" $ \t -> do
    bs <- maybe (fail "BlockHash: invalid base58") pure (fromBase58String (Text.unpack t))
    if S.length bs == 32
      then pure (BlockHash bs)
      else fail "BlockHash: expected 32 bytes"

-- | Build a 'BlockHash' from Base58 text. Calls 'error' if the text is not
-- valid Base58; the decoded length is not checked.
unsafeBlockHash :: String -> BlockHash
unsafeBlockHash = BlockHash . fromJust . fromBase58String

-- | The number of blocks beneath a block (its height in the chain).
type BlockHeight = Word64