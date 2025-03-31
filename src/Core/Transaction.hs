{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Core.Transaction where

import Core.Crypto (SolanaSignature)
import Core.Message
import Data.Aeson.Types
import GHC.Generics

data Transaction = Transaction
  { message :: CompiledMessage,
    signatures :: [SolanaSignature]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)