{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module      : Network.Solana.Core.Transaction
-- Description : A Solana transaction: a compiled message plus its signatures.
module Network.Solana.Core.Transaction where

import Data.Aeson.Types
import GHC.Generics
import Network.Solana.Core.Crypto (SolanaSignature)
import Network.Solana.Core.Message

-- | A signed transaction as exchanged with RPC nodes: the compiled
-- message together with one signature per required signer.
data Transaction = Transaction
  { -- | The compiled transaction message that was signed.
    message :: CompiledMessage,
    -- | Ed25519 signatures over the serialized message, one per required
    -- signer, in the order of the message's signer account keys.
    signatures :: [SolanaSignature]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)