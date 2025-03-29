module Core.Transaction where

import Core.Crypto (SolanaSignature)
import Core.Message

data Transaction = Transaction
  { message :: Message,
    signatures :: [SolanaSignature]
  }