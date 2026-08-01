{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Upgradeable BPF Loader program (loader-v3). Instruction
-- data is bincode-encoded (u32 little-endian discriminant), like the System
-- Program; the @Write@ variant's byte vector is bincode's @Vec\<u8\>@ encoding
-- (u64 little-endian length followed by the raw bytes).
--
-- Rust's composite helpers (@create_buffer@, @deploy_with_max_program_len@)
-- are deliberately not mirrored: compose them from the System Program client
-- (createAccount) plus 'initializeBuffer' / 'deployWithMaxDataLen'.
module Network.Solana.NativePrograms.BpfLoaderUpgradeable where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Pda (findProgramAddress)
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.Sysvar qualified as Sysvar

-- | Upgradeable BPF loader program address.
bpfLoaderUpgradeableId :: SolanaPublicKey
bpfLoaderUpgradeableId = "BPFLoaderUpgradeab1e11111111111111111111111"

-- | Upgradeable BPF loader program instructions.
data UpgradeableLoaderInstruction
  = InitializeBuffer
  | Write {wOffset :: Word32, wBytes :: BS.ByteString}
  | DeployWithMaxDataLen {dMaxDataLen :: Word64}
  | Upgrade
  | SetAuthority
  | Close
  | ExtendProgram {eAdditionalBytes :: Word32}
  | SetAuthorityChecked
  deriving (Eq, Show, Generic)

-- bincode encodes a Rust Vec<u8> as a u64 little-endian byte length
-- followed by the raw bytes.
putBincodeBytes :: BS.ByteString -> Put
putBincodeBytes bs = do
  putWord64le (fromIntegral (BS.length bs))
  putByteString bs

-- | bincode decodes a Rust Vec<u8> as a u64 little-endian byte length
-- followed by that many raw bytes.
getBincodeBytes :: Get BS.ByteString
getBincodeBytes = do
  len <- getWord64le
  if len > fromIntegral (maxBound :: Int)
    then fail "getBincodeBytes: length exceeds Int range"
    else getByteString (fromIntegral len)

instance Binary UpgradeableLoaderInstruction where
  put :: UpgradeableLoaderInstruction -> Put
  put InitializeBuffer = putWord32le 0
  put (Write offset bytes) = do
    putWord32le 1
    putWord32le offset
    putBincodeBytes bytes
  put (DeployWithMaxDataLen maxDataLen) = do
    putWord32le 2
    putWord64le maxDataLen
  put Upgrade = putWord32le 3
  put SetAuthority = putWord32le 4
  put Close = putWord32le 5
  put (ExtendProgram additionalBytes) = do
    putWord32le 6
    putWord32le additionalBytes
  put SetAuthorityChecked = putWord32le 7

  get :: Get UpgradeableLoaderInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> pure InitializeBuffer
      1 -> Write <$> getWord32le <*> getBincodeBytes
      2 -> DeployWithMaxDataLen <$> getWord64le
      3 -> pure Upgrade
      4 -> pure SetAuthority
      5 -> pure Close
      6 -> ExtendProgram <$> getWord32le
      7 -> pure SetAuthorityChecked
      _ -> fail ("UpgradeableLoaderInstruction: unknown discriminant " <> show disc)

-- | Derives the ProgramData account address for a given program: the PDA of
-- @[program]@ under the upgradeable BPF loader.
programDataAddress :: SolanaPublicKey -> Maybe SolanaPublicKey
programDataAddress prog = fst <$> findProgramAddress [getSolanaPublicKeyRaw prog] bpfLoaderUpgradeableId

-- | Creates instruction to "Initialize a Buffer account"
-- Receives the uninitialized buffer account and its authority.
-- # Account references
-- 0. `[WRITE]` Source account to initialize
-- 1. `[]` Buffer authority
initializeBuffer :: SolanaPublicKey -> SolanaPublicKey -> Instruction
initializeBuffer buffer authority =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = buffer, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = authority, isSigner = False, isWritable = False}
    ]
    InitializeBuffer

-- | Creates instruction to "Write program data into a Buffer account"
-- Receives the buffer account, its authority (which must sign), the byte
-- offset and the bytes to write.
-- # Account references
-- 0. `[WRITE]` Buffer account to write to
-- 1. `[SIGNER]` Buffer authority
write :: SolanaPublicKey -> SolanaPublicKey -> Word32 -> BS.ByteString -> Instruction
write buffer authority offset bytes =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = buffer, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
    ]
    (Write offset bytes)

-- | Creates instruction to "Deploy an executable program"
-- Receives the payer, the uninitialized ProgramData account, the
-- uninitialized Program account, the Buffer account holding the deployed
-- program data, the authority (which must sign) and the maximum length in
-- bytes the program can be upgraded to.
-- # Account references
-- 0. `[WRITE, SIGNER]` Payer account
-- 1. `[WRITE]` ProgramData account
-- 2. `[WRITE]` Program account
-- 3. `[WRITE]` Buffer account
-- 4. `[]` Rent sysvar
-- 5. `[]` Clock sysvar
-- 6. `[]` System program
-- 7. `[SIGNER]` Authority
deployWithMaxDataLen :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction
deployWithMaxDataLen payer programdata program buffer authority maxDataLen =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True},
      AccountMeta {accountPubKey = programdata, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = program, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = buffer, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = SP.systemProgramId, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
    ]
    (DeployWithMaxDataLen maxDataLen)

-- | Creates instruction to "Upgrade a program"
-- Receives the ProgramData account, the Program account, the Buffer account
-- holding the new program data, the spill account (which receives excess
-- lamports) and the authority (which must sign).
-- # Account references
-- 0. `[WRITE]` ProgramData account
-- 1. `[WRITE]` Program account
-- 2. `[WRITE]` Buffer account
-- 3. `[WRITE]` Spill account
-- 4. `[]` Rent sysvar
-- 5. `[]` Clock sysvar
-- 6. `[SIGNER]` Authority
upgrade :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
upgrade programdata program buffer spill authority =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = programdata, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = program, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = buffer, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = spill, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
    ]
    Upgrade

-- | Creates instruction to "Set a new authority"
-- Receives the account whose authority is being changed, the current
-- authority (which must sign) and the new authority.
-- # Account references
-- 0. `[WRITE]` Buffer or ProgramData account
-- 1. `[SIGNER]` Current authority
-- 2. `[]` New authority
setAuthority :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
setAuthority owned currentAuthority newAuthority =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = owned, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = newAuthority, isSigner = False, isWritable = False}
    ]
    SetAuthority

-- | Creates instruction to "Set a new authority", checking that the new
-- authority signs (a safer version of 'setAuthority').
-- Receives the account whose authority is being changed, the current
-- authority (which must sign) and the new authority (which must sign).
-- # Account references
-- 0. `[WRITE]` Buffer or ProgramData account
-- 1. `[SIGNER]` Current authority
-- 2. `[SIGNER]` New authority
setAuthorityChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
setAuthorityChecked owned currentAuthority newAuthority =
  mkInstruction
    bpfLoaderUpgradeableId
    [ AccountMeta {accountPubKey = owned, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = newAuthority, isSigner = True, isWritable = False}
    ]
    SetAuthorityChecked

-- | Creates instruction to "Close an account"
-- Receives the account to close, the recipient of its lamports, the
-- authority (which must sign) and, when closing a ProgramData account, the
-- associated Program account.
-- # Account references
-- 0. `[WRITE]` Account to close
-- 1. `[WRITE]` Recipient account
-- 2. `[SIGNER]` Authority
-- 3. `[WRITE]` (optional) Associated Program account, if closing a ProgramData account
closeAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey -> Instruction
closeAccount account recipient authority mProgram =
  mkInstruction
    bpfLoaderUpgradeableId
    ( [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = recipient, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
      ]
        ++ maybe [] (\p -> [AccountMeta {accountPubKey = p, isSigner = False, isWritable = True}]) mProgram
    )
    Close

-- | Creates instruction to "Extend a program's ProgramData account by the
-- given number of bytes"
-- Receives the ProgramData account, the Program account, an optional payer
-- to fund the extension (the system program is always included alongside
-- it), and the number of additional bytes.
-- # Account references
-- 0. `[WRITE]` ProgramData account
-- 1. `[WRITE]` Program account
-- 2. `[]` (optional) System program
-- 3. `[WRITE, SIGNER]` (optional) Payer account
extendProgram :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey -> Word32 -> Instruction
extendProgram programdata program mPayer additionalBytes =
  mkInstruction
    bpfLoaderUpgradeableId
    ( [ AccountMeta {accountPubKey = programdata, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = program, isSigner = False, isWritable = True}
      ]
        ++ maybe
          []
          ( \payer ->
              [ AccountMeta {accountPubKey = SP.systemProgramId, isSigner = False, isWritable = False},
                AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True}
              ]
          )
          mPayer
    )
    (ExtendProgram additionalBytes)
