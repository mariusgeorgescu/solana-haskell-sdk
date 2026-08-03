{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.NativePrograms.SystemProgram where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Block (BlockHash)
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
        -- | String of ASCII chars, no longer than 'Network.Solana.Constants.maxSeedLen'
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
  | -- | Allocate space in a (possibly new) account without funding
    Allocate
      { -- | Number of bytes of memory to allocate
        space :: Word64
      }
  | -- | Allocate space for and assign an account at an address derived from a base public key and a seed
    AllocateWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than 'Network.Solana.Constants.maxSeedLen'
        seed :: String,
        -- | Number of bytes of memory to allocate
        space :: Word64,
        -- | Address of program that will own the account
        owner :: SolanaPublicKey
      }
  | -- | Assign account to a program based on a seed
    AssignWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than 'Network.Solana.Constants.maxSeedLen'
        seed :: String,
        -- | Owner program account
        owner :: SolanaPublicKey
      }
  | -- | Transfer lamports from a derived address
    TransferWithSeed
      { -- | Transfer amount
        lamports :: Word64,
        -- | Seed to use to derive the funding account address
        fromSeed :: String,
        -- | Owner to use to derive the funding account address
        fromOwner :: SolanaPublicKey
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
  put (Allocate space) = do
    putWord32le (8 :: Word32)
    putWord64le space
  put (AllocateWithSeed base seed space owner) = do
    putWord32le (9 :: Word32)
    putByteString (getSolanaPublicKeyRaw base)
    putBincodeString seed
    putWord64le space
    putByteString (getSolanaPublicKeyRaw owner)
  put (AssignWithSeed base seed owner) = do
    putWord32le (10 :: Word32)
    putByteString (getSolanaPublicKeyRaw base)
    putBincodeString seed
    putByteString (getSolanaPublicKeyRaw owner)
  put (TransferWithSeed lamports fromSeed fromOwner) = do
    putWord32le (11 :: Word32)
    putWord64le lamports
    putBincodeString fromSeed
    putByteString (getSolanaPublicKeyRaw fromOwner)
  put UpgradeNonceAccount =
    putWord32le (12 :: Word32)

  get :: Get SystemInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> CreateAccount <$> getWord64le <*> getWord64le <*> get
      1 -> Assign <$> get
      2 -> Transfer <$> getWord64le
      3 -> CreateAccountWithSeed <$> get <*> getBincodeString <*> getWord64le <*> getWord64le <*> get
      4 -> pure AdvanceNonceAccount
      5 -> WithdrawNonceAccount <$> getWord64le
      6 -> InitializeNonceAccount <$> get
      7 -> AuthorizeNonceAccount <$> get
      8 -> Allocate <$> getWord64le
      9 -> AllocateWithSeed <$> get <*> getBincodeString <*> getWord64le <*> get
      10 -> AssignWithSeed <$> get <*> getBincodeString <*> get
      11 -> TransferWithSeed <$> getWord64le <*> getBincodeString <*> get
      12 -> pure UpgradeNonceAccount
      _ -> fail ("SystemInstruction: unknown discriminant " <> show disc)

-- bincode encodes a Rust String as a u64 little-endian byte length
-- followed by the UTF-8 bytes
putBincodeString :: String -> Put
putBincodeString s = do
  let bs = TE.encodeUtf8 (T.pack s)
  putWord64le (fromIntegral (BS.length bs))
  putByteString bs

-- | bincode decodes a Rust String as a u64 little-endian byte length
-- followed by that many UTF-8 bytes.
getBincodeString :: Get String
getBincodeString = do
  len <- getWord64le
  if len > fromIntegral (maxBound :: Int)
    then fail "getBincodeString: length exceeds Int range"
    else do
      bs <- getByteString (fromIntegral len)
      case TE.decodeUtf8' bs of
        Left _ -> fail "getBincodeString: invalid UTF-8"
        Right t -> pure (T.unpack t)

-- | A nonce account's on-chain state (bincode-encoded @Versions<State>@:
-- a u32 version tag around a u32 state tag).
data NonceState
  = NonceUninitialized
  | NonceInitialized
      { nsAuthority :: SolanaPublicKey,
        nsDurableNonce :: BlockHash,
        nsLamportsPerSignature :: Word64
      }
  deriving (Eq, Show)

-- | Decodes a nonce account's data: a u32 version (0 or 1), then a u32 state
-- discriminant (0 uninitialized, 1 initialized with authority, durable
-- nonce, and fee-rate fields). Trailing bytes (account padding) are allowed.
decodeNonceAccount :: BS.ByteString -> Either String NonceState
decodeNonceAccount bs = case runGetOrFail getNonceState (BL.fromStrict bs) of
  Left (_, _, err) -> Left err
  Right (_, _, ns) -> Right ns
  where
    getNonceState = do
      version <- getWord32le
      if version /= 0 && version /= 1
        then fail ("NonceState: unsupported version " <> show version)
        else do
          st <- getWord32le
          case st of
            0 -> pure NonceUninitialized
            1 -> NonceInitialized <$> get <*> get <*> getWord64le
            _ -> fail ("NonceState: unknown state " <> show st)

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

-- | Creates instruction to "Allocate space in a (possibly new) account without funding"
-- Receives the account and the number of bytes of memory to allocate.
-- # Account references
-- 0. `[WRITE, SIGNER]` New account
allocate :: SolanaPublicKey -> Int -> Instruction
allocate newAccount space =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = newAccount, isSigner = True, isWritable = True}
    ]
    (Allocate (fromIntegral space))

-- | Creates instruction to "Allocate space for and assign an account at an address derived from a base public key and a seed"
-- Receives the allocated account, base account, seed, the number of bytes and the owner program account.
-- # Account references
-- 0. `[WRITE]` Allocated account
-- 1. `[SIGNER]` Base account
allocateWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> Int -> SolanaPublicKey -> Instruction
allocateWithSeed allocatedAccount baseAccount seed space ownerProgramAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = allocatedAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = baseAccount, isSigner = True, isWritable = False}
    ]
    (AllocateWithSeed baseAccount seed (fromIntegral space) ownerProgramAccount)

-- | Creates instruction to "Assign account to a program based on a seed"
-- Receives the assigned account, base account, seed and the owner program account.
-- # Account references
-- 0. `[WRITE]` Assigned account
-- 1. `[SIGNER]` Base account
assignWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> SolanaPublicKey -> Instruction
assignWithSeed assignedAccount baseAccount seed ownerProgramAccount =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = assignedAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = baseAccount, isSigner = True, isWritable = False}
    ]
    (AssignWithSeed baseAccount seed ownerProgramAccount)

-- | Creates instruction to "Transfer lamports from a derived address"
-- Receives the funding account, base account, seed, the owner of the funding account, the recipient and the amount.
-- # Account references
-- 0. `[WRITE]` Funding account
-- 1. `[SIGNER]` Base for funding account
-- 2. `[WRITE]` Recipient account
transferWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction
transferWithSeed fundingAccount baseAccount fromSeed fromOwner recipientAccount amount =
  mkInstruction
    systemProgramId
    [ AccountMeta {accountPubKey = fundingAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = baseAccount, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = recipientAccount, isSigner = False, isWritable = True}
    ]
    (TransferWithSeed (fromIntegral amount) fromSeed fromOwner)