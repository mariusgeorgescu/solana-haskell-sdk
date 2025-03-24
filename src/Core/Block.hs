{-# LANGUAGE DerivingVia #-}

module Core.Block where

import Data.Binary
import Data.ByteString qualified as S
import GHC.Generics (Generic)

newtype BlockHash = BlockHash S.ByteString
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)

instance Binary BlockHash where
  put :: BlockHash -> Put
  put (BlockHash bs) = put bs
  get :: Get BlockHash
  get = BlockHash <$> get