{-# LANGUAGE DerivingVia #-}

module Core.Block where

import Data.ByteString qualified as S
import GHC.Generics (Generic)

newtype BlockHash = BlockHash S.ByteString
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup)
  deriving newtype (Monoid)