{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Network.Solana.Core.Transaction where

import Data.Aeson.Types
import GHC.Generics
import Network.Solana.Core.Crypto (SolanaSignature)
import Network.Solana.Core.Message

data Transaction = Transaction
  { message :: CompiledMessage,
    signatures :: [SolanaSignature]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)