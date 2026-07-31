{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.NativePrograms.SystemProgram where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Sysvar qualified as Sysvar

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
  | -- | Consume a stored nonce, replacing it with a successor
    AdvanceNonceAccount
  | -- | Withdraw funds from a nonce account
    WithdrawNonceAccount
      { -- | Withdraw amount
        lamports :: Word64
      }
  | -- | Drive state of Uninitialized nonce account to Initialized, setting the nonce value
    InitializeNonceAccount
      { -- | Entity authorized to execute nonce instructions on the account
        authority :: SolanaPublicKey
      }
  | -- | Change the entity authorized to execute nonce instructions on the account
    AuthorizeNonceAccount
      { -- | New nonce authority
        authority :: SolanaPublicKey
      }
  | -- | One-time idempotent upgrade of legacy nonce versions to bump them out of chain blockhash domain
    UpgradeNonceAccount
  deriving (Eq, Show, Generic)

instance Binary SystemInstruction where
  put :: SystemInstruction -> Put
  put (CreateAccount lamports space owner) = do
    putWord32le (0 :: Word32)
    putWord64le lamports
    putWord64le space
    putByteString (getSolanaPublicKeyRaw owner)
  put (Assign owner) = do
    putWord32le (1 :: Word32)
    putByteString (getSolanaPublicKeyRaw owner)
  put (Transfer lamports) = do
    putWord32le (2 :: Word32)
    putWord64le lamports
  put (CreateAccountWithSeed base seed lamports space owner) = do
    putWord32le (3 :: Word32)
    putByteString (getSolanaPublicKeyRaw base)
    putBincodeString seed
    putWord64le lamports
    putWord64le space
    putByteString (getSolanaPublicKeyRaw owner)
  put AdvanceNonceAccount =
    putWord32le (4 :: Word32)
  put (WithdrawNonceAccount lamports) = do
    putWord32le (5 :: Word32)
    putWord64le lamports
  put (InitializeNonceAccount authority) = do
    putWord32le (6 :: Word32)
    putByteString (getSolanaPublicKeyRaw authority)
  put (AuthorizeNonceAccount authority) = do
    putWord32le (7 :: Word32)
    putByteString (getSolanaPublicKeyRaw authority)
  put UpgradeNonceAccount =
    putWord32le (12 :: Word32)

-- bincode encodes a Rust String as a u64 little-endian byte length
-- followed by the UTF-8 bytes
putBincodeString :: String -> Put
putBincodeString s = do
  let bs = TE.encodeUtf8 (T.pack s)
  putWord64le (fromIntegral (BS.length bs))
  putByteString bs

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

-- | Creates instruction to "Consume a stored nonce, replacing it with a successor"
-- Receives the nonce account and the nonce authority.
-- # Account references
-- 0. `[WRITE]` Nonce account
-- 1. `[]` RecentBlockhashes sysvar
-- 2. `[SIGNER]` Nonce authority
advanceNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction
advanceNonceAccount nonceAccount nonceAuthority =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = nonceAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = nonceAuthority, isSigner = True, isWritable = False}
    ]
    AdvanceNonceAccount

-- | Creates instruction to "Withdraw funds from a nonce account"
-- Receives the nonce account, the nonce authority, the recipient account and the amount.
-- # Account references
-- 0. `[WRITE]` Nonce account
-- 1. `[WRITE]` Recipient account
-- 2. `[]` RecentBlockhashes sysvar
-- 3. `[]` Rent sysvar
-- 4. `[SIGNER]` Nonce authority
withdrawNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction
withdrawNonceAccount nonceAccount nonceAuthority recipientAccount amount =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = nonceAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = recipientAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = nonceAuthority, isSigner = True, isWritable = False}
    ]
    (WithdrawNonceAccount (fromIntegral amount))

-- | Creates instruction to "Drive state of Uninitialized nonce account to Initialized"
-- Receives the nonce account and the entity authorized to execute nonce instructions on it.
-- # Account references
-- 0. `[WRITE]` Nonce account
-- 1. `[]` RecentBlockhashes sysvar
-- 2. `[]` Rent sysvar
initializeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction
initializeNonceAccount nonceAccount nonceAuthority =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = nonceAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False}
    ]
    (InitializeNonceAccount nonceAuthority)

-- | Creates instruction to "Change the entity authorized to execute nonce instructions on the account"
-- Receives the nonce account, the current nonce authority and the new nonce authority.
-- # Account references
-- 0. `[WRITE]` Nonce account
-- 1. `[SIGNER]` Nonce authority
authorizeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
authorizeNonceAccount nonceAccount nonceAuthority newAuthority =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = nonceAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = nonceAuthority, isSigner = True, isWritable = False}
    ]
    (AuthorizeNonceAccount newAuthority)

-- | Creates instruction to "Upgrade legacy nonce versions"
-- Receives the nonce account.
-- # Account references
-- 0. `[WRITE]` Nonce account
upgradeNonceAccount :: SolanaPublicKey -> Instruction
upgradeNonceAccount nonceAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = nonceAccount, isSigner = False, isWritable = True}
    ]
    UpgradeNonceAccount