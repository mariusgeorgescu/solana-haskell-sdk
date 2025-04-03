{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.NativePrograms.SystemProgram
-- Description : Implementation of Solana's System Program instructions and serialization.
-- Copyright   : (c) 2024
-- This module provides Haskell representations of the Solana System Program's instructions,
-- along with Binary instances to serialize these instructions for transaction creation.
-- The System Program allows creating accounts, transferring lamports, assigning account ownership,
-- allocating storage, and handling nonce accounts.
module Network.Solana.NativePrograms.SystemProgram where

import Data.Binary
import Data.Binary.Put
import GHC.Generics
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Sysvar qualified as Sysvars

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
  | -- | Consumes a stored nonce, replacing it with a successor
    AdvanceNonceAccount
  | -- | Withdraw funds from a nonce account
    WithdrawNonceAccount
      { -- | Lamports to withdraw, which must leave the account balance above the rent exempt reserve or at zero.
        lamports :: Word64
      }
  | -- | Drive state of Uninitialized nonce account to Initialized, setting the nonce value
    InitializeNonceAccount
      { -- | Specifies the entity authorized to execute nonce instruction on the account
        authorized :: SolanaPublicKey
      }
  | -- | Change the entity authorized to execute nonce instructions on the account
    AuthorizeNonceAccount
      { -- |  identifies the entity to authorizeF
        authorized :: SolanaPublicKey
      }
  | -- |  Allocate space in a (possibly new) account without funding
    Allocate
      { -- |  Number of bytes of memory to allocate
        space :: Word64
      }
  | -- | Allocate space for and assign an account at an address derived from a base public key and a seed
    AllocateWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than `maxSeedLen`
        seed :: String,
        -- |  Number of bytes of memory to allocate
        space :: Word64,
        -- |  Owner program account
        owner :: SolanaPublicKey
      }
  | -- | Assign account to a program based on a seed
    AssignWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than `maxSeedLen`
        seed :: String,
        -- | Owner program account
        owner :: SolanaPublicKey
      }
  | -- |  Transfer lamports from a derived address
    TransferWithSeed
      { -- | Amount of lamports to transfer
        lamports :: Word64,
        -- | Seed to use to derive the funding account address
        fromSeed :: String,
        -- | Owner to use to derive the funding account address
        fromOwner :: SolanaPublicKey
      }
  | UpgradeNonceAccount
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
  put AdvanceNonceAccount = putWord32le 4
  put (WithdrawNonceAccount lamports) = do
    putWord32le 5
    putWord64le lamports
  put (InitializeNonceAccount authorized) = do
    putWord32le 6
    put (getSolanaPublicKeyRaw authorized)
  put (AuthorizeNonceAccount authorized) = do
    putWord32le 7
    put (getSolanaPublicKeyRaw authorized)
  put (Allocate space) = do
    putWord32le 8
    putWord64le space
  put (AllocateWithSeed base seed space owner) = do
    putWord32le 9
    put (getSolanaPublicKeyRaw base)
    putStringUtf8 seed
    putWord64le space
    put (getSolanaPublicKeyRaw owner)
  put (AssignWithSeed base seed owner) = do
    putWord32le 10
    put (getSolanaPublicKeyRaw base)
    putStringUtf8 seed
    put (getSolanaPublicKeyRaw owner)
  put (TransferWithSeed lamports fromSeed fromOwner) = do
    putWord32le 11
    putWord64le lamports
    putStringUtf8 fromSeed
    put (getSolanaPublicKeyRaw fromOwner)
  put UpgradeNonceAccount = putWord32le 12

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

-- | Creates an instruction to "Advance the state of a Nonce account"
-- Receives the nonce account and the authorized public key.
-- Consumes a stored nonce, replacing it with a successor
-- # Account references
-- 0. `[WRITE]` Nonce account
-- 1. `[]` RecentBlockhashes sysvar
-- 2. `[SIGNER]` Nonce authority
advanceNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction
advanceNonceAccount nonceAccount authorizedPubkey =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = nonceAccount,
          isSigner = False,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = Sysvars.recentBlockhashes,
          isSigner = False,
          isWritable = False
        },
      AccountMeta
        { accountPubKey = authorizedPubkey,
          isSigner = True,
          isWritable = False
        }
    ]
    AdvanceNonceAccount

-- | Creates an instruction to "Withdraw lamports from a Nonce account"
-- Receives nonce account, recipient account, authorized public key and the amount to withdraw.
--  # Account references
--    0. `[WRITE]` Nonce account
--    1. `[WRITE]` Recipient account
--    2. `[]` RecentBlockhashes sysvar
--    3. `[]` Rent sysvar
--    4. `[SIGNER]` Nonce authority
withdrawNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction
withdrawNonceAccount nonceAccount recipient authorizedPubkey lamports =
  mkInstruction
    systemProgramId
    [ AccountMeta nonceAccount False True,
      AccountMeta recipient False True,
      AccountMeta Sysvars.recentBlockhashes False False,
      AccountMeta Sysvars.rent False False,
      AccountMeta authorizedPubkey True False
    ]
    (WithdrawNonceAccount $ fromIntegral lamports)

-- | Creates an instruction to "Initialize a Nonce account"
-- Receives nonce account and the authorized public key.
-- No signatures are required to execute this instruction, enabling derived  nonce account addresses
--  # Account references
--    0. `[WRITE]` Nonce account
--    1. `[]` RecentBlockhashes sysvar
--    2. `[]` Rent sysvar
initializeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction
initializeNonceAccount nonceAccount authorizedPubkey =
  mkInstruction
    systemProgramId
    [ AccountMeta nonceAccount False True,
      AccountMeta Sysvars.recentBlockhashes False False,
      AccountMeta Sysvars.rent False False
    ]
    (InitializeNonceAccount authorizedPubkey)

-- | Creates an instruction to "Change the entity authorized to execute nonce instructions on the account"
-- Receives nonce account, current authorized public key and the new authorized public key.
--  # Account references
--    0. `[WRITE]` Nonce account
--    1. `[SIGNER]` Nonce authority
authorizeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
authorizeNonceAccount nonceAccount authorizedPubkey newAuthorizedPubkey =
  mkInstruction
    systemProgramId
    [ AccountMeta nonceAccount False True,
      AccountMeta authorizedPubkey True False
    ]
    (AuthorizeNonceAccount newAuthorizedPubkey)

-- | Creates an instruction to "Allocate space in a (possibly new) account without funding"
-- Receives account public key and number of bytes to allocate.
-- # Account references
-- 0. `[WRITE, SIGNER]` New account
allocate :: SolanaPublicKey -> Word64 -> Instruction
allocate account space =
  mkInstruction
    systemProgramId
    [ AccountMeta account True True
    ]
    (Allocate space)

-- | Creates an instruction to "Allocate space for and assign an account at an address derived from a base public key and a seed"
-- Receives account public key, base public key, seed, number of bytes to allocate and owner.
-- # Account references
--   0. `[WRITE]` Allocated account
--   1. `[SIGNER]` Base account
allocateWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> Word64 -> SolanaPublicKey -> Instruction
allocateWithSeed account base seed space owner =
  mkInstruction
    systemProgramId
    [ AccountMeta account False True,
      AccountMeta base True False
    ]
    (AllocateWithSeed base seed space owner)

-- | Creates an instruction to "Assign an account to an owner program using seed derivation"
-- Receives account public key, base public key, seed and owner program account.
-- # Account references
--   0. `[WRITE]` Assigned account
--   1. `[SIGNER]` Base account
assignWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> SolanaPublicKey -> Instruction
assignWithSeed account base seed owner =
  mkInstruction
    systemProgramId
    [ AccountMeta account False True,
      AccountMeta base True False
    ]
    (AssignWithSeed base seed owner)

-- | Transfer lamports from a derived address
--
-- # Account references
--   0. `[WRITE]` Funding account
--   1. `[SIGNER]` Base for funding account
--   2. `[WRITE]` Recipient account
transferWithSeed :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> String -> SolanaPublicKey -> Instruction
transferWithSeed fundingAccount baseAccount recipient lamports seed owner =
  mkInstruction
    systemProgramId
    [ AccountMeta fundingAccount False True,
      AccountMeta baseAccount True False,
      AccountMeta recipient False True
    ]
    (TransferWithSeed lamports seed owner)

-- | One-time idempotent upgrade of legacy nonce versions in order to bump  them out of chain blockhash domain.
-- # Account references
-- 0. `[WRITE]` Nonce account
upgradeNonceAccount :: SolanaPublicKey -> Instruction
upgradeNonceAccount nonceAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta nonceAccount False True
    ]
    UpgradeNonceAccount