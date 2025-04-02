{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.NativePrograms.SystemProgram where

import Data.Binary
import Data.Binary.Put
import GHC.Generics
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction

-- |  System program address. This program contain instructions to:
-- create new accounts, allocate account data, assign accounts to owning programs,
-- transfer lamports from System Program owned accounts and pay transaction fees.
systemProgramId :: SolanaPublicKey
systemProgramId = "11111111111111111111111111111111"

data SystemInstruction
  = -- |  Create a new account
    CreateAccount
      { -- |  Number of lamports to transfer to the new account
        lamports :: Word64,
        -- | Number of bytes of memory to allocate
        space :: Word64,
        -- | Address of program that will own the new account
        owner :: SolanaPublicKey
      }
  | -- | Assign account to a program
    Assign
      { -- | Owner program account
        owner :: SolanaPublicKey
      }
  | -- | Transfer lamports
    Transfer
      { -- | Transfer amount
        lamports :: Word64
      }
  | CreateAccountWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than `maxSeedLen`
        seed :: String,
        -- |  Number of lamports to transfer to the new account
        lamports :: Word64,
        -- | Number of bytes of memory to allocate
        space :: Word64,
        -- | Address of program that will own the new account
        owner :: SolanaPublicKey
      }
  deriving (Eq, Show, Generic)

instance Binary SystemInstruction where
  put :: SystemInstruction -> Put
  put (CreateAccount lamports space owner) = do
    putWord32le (0 :: Word32)
    putWord64le lamports
    putWord64le space
    put (getSolanaPublicKeyRaw owner)
  put (Assign owner) =
    do
      putWord32le (1 :: Word32)
      put (getSolanaPublicKeyRaw owner)
  put (Transfer lamports) =
    do
      putWord32le (2 :: Word32)
      putWord64le lamports
  put (CreateAccountWithSeed base seed lamports space owner) =
    do
      putWord32le (3 :: Word32)
      put (getSolanaPublicKeyRaw base)
      putStringUtf8 seed
      putWord64le lamports
      putWord64le space
      put (getSolanaPublicKeyRaw owner)

-- | Creates instruction to "Create a new account"
-- Receives the funding account, the new account, amount to transfer, the number of bytes of memory to allocate and the owner program account.
--   # Account references
--   0. `[WRITE, SIGNER]` Funding account
--   1. `[WRITE, SIGNER]` New account
createAccount :: SolanaPublicKey -> SolanaPublicKey -> Lamport -> Int -> SolanaPublicKey -> Instruction
createAccount fundingAccount newAccount lamports space ownerProgramAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = fundingAccount,
          isSigner = True,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = newAccount,
          isSigner = True,
          isWritable = True
        }
    ]
    (CreateAccount (fromIntegral lamports) (fromIntegral space) ownerProgramAccount)

-- | Creates instruction to "Assign account to a program"
-- Receives addresses for the assigned account and owner program account
-- # Account references
-- 0. `[WRITE, SIGNER]` Assigned account public key
assignAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction
assignAccount assignedAccount ownerProgramAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = assignedAccount,
          isSigner = True,
          isWritable = True
        }
    ]
    (Assign ownerProgramAccount)

-- | Creates instruction to  "Transfer lamports"
-- Receives the funding account, the recipient account and the amount to transfer.
--  # Account references
--  0. `[WRITE, SIGNER]` Funding account
--  1. `[WRITE]` Recipient account
transfer :: SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction
transfer fundingAccount recipientAccount amount =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = fundingAccount,
          isSigner = True,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = recipientAccount,
          isSigner = False,
          isWritable = True
        }
    ]
    (Transfer (fromIntegral amount))

-- | Creates instruction to "Create a new account at an address derived from a base pubkey and a seed"
-- Receives the funding account, the new account, base pubkey, seed, amount to transfer, the number of bytes of memory to allocate and the owner program account.
-- # Account references
-- 0. `[WRITE, SIGNER]` Funding account
-- 1. `[WRITE]` Created account
-- 2. `[SIGNER]` (optional) Base account; the account matching the base Pubkey below must be
--      provided as a signer, but may be the same as the funding account and provided as account 0
createAccountWithSeed :: SolanaPublicKey -> String -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Int -> SolanaPublicKey -> Instruction
createAccountWithSeed baseAccount seed fundingAccount newAccount lamports space ownerProgramAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = fundingAccount,
          isSigner = True,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = newAccount,
          isSigner = False,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = baseAccount,
          isSigner = True,
          isWritable = False
        }
    ]
    (CreateAccountWithSeed baseAccount seed (fromIntegral lamports) (fromIntegral space) ownerProgramAccount)