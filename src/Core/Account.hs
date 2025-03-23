module Core.Account where

import Core.Crypto (SolanaPublicKey)
import Data.ByteString qualified as S
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Lamport is the smallest unit of SOL (1 SOL = 1 billion lamports).
newtype Lamport = Lamport
  { unLamport :: Word64
  }
  deriving (Eq, Ord, Show, Num, Generic)

{- Solana Account structure.
    Accounts have a max size of 10MiB and every account on Solana has the same base Account type.
-}
data Account = Account
  { -- | Amount of lamports in the account
    lamports :: Lamport,
    {-  A byte array that stores arbitrary data for an account.
        For non-executable accounts, this generally stores state that is meant to be read-only.
        For program accounts (smart contracts), this contains the executable program code.
        The data field is commonly referred to as "account data".
    -}
    dataField :: S.ByteString,
    {- The  program ID  'SolanaPublicKey' associated with the program that owns this account.
       Only the owner program can modify the account's data or deduct its lamports balance.
    -}
    owner :: SolanaPublicKey,
    -- |   A boolean flag that indicates whether this account contains a loaded program.
    executable :: Bool
  }
  deriving (Show, Eq, Generic)
