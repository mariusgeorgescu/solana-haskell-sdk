{-# LANGUAGE OverloadedStrings #-}

-- | Client for the SPL Token program: <https://spl.solana.com/token>
-- Instruction data uses SPL Token's hand-rolled pack format (u8 discriminant,
-- little-endian integers, raw 32-byte pubkeys, and optional pubkeys encoded
-- as a 1-byte tag followed by the key when present) — this is NOT bincode.
module Network.Solana.SplPrograms.Token where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Sysvar qualified as Sysvar

-- | SPL Token program address.
tokenProgramId :: SolanaPublicKey
tokenProgramId = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

-- | Authorities a token mint or account can carry (SPL @AuthorityType@, u8 0-3).
data AuthorityType
  = MintTokens
  | FreezeAuthority
  | AccountOwner
  | CloseAuthority
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | SPL Token instructions 0-20 (instructions 21-24 are deferred post-1.0).
data TokenInstruction
  = InitializeMint {tiDecimals :: Word8, tiMintAuthority :: SolanaPublicKey, tiFreezeAuthority :: Maybe SolanaPublicKey}
  | InitializeAccount
  | InitializeMultisig {tiM :: Word8}
  | Transfer {tiAmount :: Word64}
  | Approve {tiAmount :: Word64}
  | Revoke
  | SetAuthority {tiAuthorityType :: AuthorityType, tiNewAuthority :: Maybe SolanaPublicKey}
  | MintTo {tiAmount :: Word64}
  | Burn {tiAmount :: Word64}
  | CloseAccount
  | FreezeAccount
  | ThawAccount
  | TransferChecked {tiAmount :: Word64, tiDecimals :: Word8}
  | ApproveChecked {tiAmount :: Word64, tiDecimals :: Word8}
  | MintToChecked {tiAmount :: Word64, tiDecimals :: Word8}
  | BurnChecked {tiAmount :: Word64, tiDecimals :: Word8}
  | InitializeAccount2 {tiOwner :: SolanaPublicKey}
  | SyncNative
  | InitializeAccount3 {tiOwner :: SolanaPublicKey}
  | InitializeMultisig2 {tiM :: Word8}
  | InitializeMint2 {tiDecimals :: Word8, tiMintAuthority :: SolanaPublicKey, tiFreezeAuthority :: Maybe SolanaPublicKey}
  deriving (Eq, Show, Generic)

putCOptionPubkey :: Maybe SolanaPublicKey -> Put
putCOptionPubkey Nothing = putWord8 0
putCOptionPubkey (Just pk) = do
  putWord8 1
  putByteString (getSolanaPublicKeyRaw pk)

getCOptionPubkey :: Get (Maybe SolanaPublicKey)
getCOptionPubkey = do
  tag <- getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> get
    _ -> fail ("COption<Pubkey>: invalid tag " <> show tag)

instance Binary TokenInstruction where
  put :: TokenInstruction -> Put
  put (InitializeMint decimals mintAuth freezeAuth) = do
    putWord8 0
    putWord8 decimals
    putByteString (getSolanaPublicKeyRaw mintAuth)
    putCOptionPubkey freezeAuth
  put InitializeAccount = putWord8 1
  put (InitializeMultisig m) = do
    putWord8 2
    putWord8 m
  put (Transfer amount) = do
    putWord8 3
    putWord64le amount
  put (Approve amount) = do
    putWord8 4
    putWord64le amount
  put Revoke = putWord8 5
  put (SetAuthority authType newAuth) = do
    putWord8 6
    putWord8 (fromIntegral (fromEnum authType))
    putCOptionPubkey newAuth
  put (MintTo amount) = do
    putWord8 7
    putWord64le amount
  put (Burn amount) = do
    putWord8 8
    putWord64le amount
  put CloseAccount = putWord8 9
  put FreezeAccount = putWord8 10
  put ThawAccount = putWord8 11
  put (TransferChecked amount decimals) = do
    putWord8 12
    putWord64le amount
    putWord8 decimals
  put (ApproveChecked amount decimals) = do
    putWord8 13
    putWord64le amount
    putWord8 decimals
  put (MintToChecked amount decimals) = do
    putWord8 14
    putWord64le amount
    putWord8 decimals
  put (BurnChecked amount decimals) = do
    putWord8 15
    putWord64le amount
    putWord8 decimals
  put (InitializeAccount2 owner) = do
    putWord8 16
    putByteString (getSolanaPublicKeyRaw owner)
  put SyncNative = putWord8 17
  put (InitializeAccount3 owner) = do
    putWord8 18
    putByteString (getSolanaPublicKeyRaw owner)
  put (InitializeMultisig2 m) = do
    putWord8 19
    putWord8 m
  put (InitializeMint2 decimals mintAuth freezeAuth) = do
    putWord8 20
    putWord8 decimals
    putByteString (getSolanaPublicKeyRaw mintAuth)
    putCOptionPubkey freezeAuth

  get :: Get TokenInstruction
  get = do
    disc <- getWord8
    case disc of
      0 -> InitializeMint <$> getWord8 <*> get <*> getCOptionPubkey
      1 -> pure InitializeAccount
      2 -> InitializeMultisig <$> getWord8
      3 -> Transfer <$> getWord64le
      4 -> Approve <$> getWord64le
      5 -> pure Revoke
      6 -> SetAuthority <$> getAuthorityType <*> getCOptionPubkey
      7 -> MintTo <$> getWord64le
      8 -> Burn <$> getWord64le
      9 -> pure CloseAccount
      10 -> pure FreezeAccount
      11 -> pure ThawAccount
      12 -> TransferChecked <$> getWord64le <*> getWord8
      13 -> ApproveChecked <$> getWord64le <*> getWord8
      14 -> MintToChecked <$> getWord64le <*> getWord8
      15 -> BurnChecked <$> getWord64le <*> getWord8
      16 -> InitializeAccount2 <$> get
      17 -> pure SyncNative
      18 -> InitializeAccount3 <$> get
      19 -> InitializeMultisig2 <$> getWord8
      20 -> InitializeMint2 <$> getWord8 <*> get <*> getCOptionPubkey
      _ -> fail ("TokenInstruction: unknown discriminant " <> show disc)
    where
      getAuthorityType = do
        b <- getWord8
        if b <= 3
          then pure (toEnum (fromIntegral b))
          else fail ("AuthorityType: invalid value " <> show b)

-- | Rent sysvar (re-exported for account-meta lists).
rentSysvar :: SolanaPublicKey
rentSysvar = Sysvar.rent

-- Owner-or-multisig meta convention (mirrors the Rust @spl-token@ builders):
-- with an empty signer list the owner itself signs; with a non-empty list the
-- owner is a readonly non-signer (the multisig account) and each listed key
-- is a readonly signer.
ownerMetas :: SolanaPublicKey -> [SolanaPublicKey] -> [AccountMeta]
ownerMetas owner signers =
  AccountMeta {accountPubKey = owner, isSigner = null signers, isWritable = False}
    : [AccountMeta {accountPubKey = s, isSigner = True, isWritable = False} | s <- signers]

-- | Initialize a new mint. Accounts: 0. `[WRITE]` mint, 1. `[]` rent sysvar.
initializeMint :: SolanaPublicKey -> Word8 -> SolanaPublicKey -> Maybe SolanaPublicKey -> Instruction
initializeMint mint decimals mintAuthority freezeAuthority =
  mkInstruction
    tokenProgramId
    [ AccountMeta {accountPubKey = mint, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = rentSysvar, isSigner = False, isWritable = False}
    ]
    (InitializeMint decimals mintAuthority freezeAuthority)

-- | Initialize a token account. Accounts: 0. `[WRITE]` account, 1. `[]` mint, 2. `[]` owner, 3. `[]` rent sysvar.
initializeAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
initializeAccount account mint owner =
  mkInstruction
    tokenProgramId
    [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = owner, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = rentSysvar, isSigner = False, isWritable = False}
    ]
    InitializeAccount

-- | Transfer tokens. Accounts: 0. `[WRITE]` source, 1. `[WRITE]` destination,
-- then owner-or-multisig metas.
transfer :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Instruction
transfer source destination owner signers amount =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = source, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = destination, isSigner = False, isWritable = True}
      ]
        <> ownerMetas owner signers
    )
    (Transfer amount)

-- | Approve a delegate. Accounts: 0. `[WRITE]` source, 1. `[]` delegate, then owner metas.
approve :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Instruction
approve source delegate owner signers amount =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = source, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = delegate, isSigner = False, isWritable = False}
      ]
        <> ownerMetas owner signers
    )
    (Approve amount)

-- | Revoke a delegate. Accounts: 0. `[WRITE]` source, then owner metas.
revoke :: SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Instruction
revoke source owner signers =
  mkInstruction
    tokenProgramId
    (AccountMeta {accountPubKey = source, isSigner = False, isWritable = True} : ownerMetas owner signers)
    Revoke

-- | Change a mint or account authority. Accounts: 0. `[WRITE]` mint/account, then owner metas.
setAuthority :: SolanaPublicKey -> AuthorityType -> Maybe SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Instruction
setAuthority owned authorityType newAuthority currentAuthority signers =
  mkInstruction
    tokenProgramId
    (AccountMeta {accountPubKey = owned, isSigner = False, isWritable = True} : ownerMetas currentAuthority signers)
    (SetAuthority authorityType newAuthority)

-- | Mint new tokens. Accounts: 0. `[WRITE]` mint, 1. `[WRITE]` destination account, then owner metas.
mintTo :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Instruction
mintTo mint destination mintAuthority signers amount =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = mint, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = destination, isSigner = False, isWritable = True}
      ]
        <> ownerMetas mintAuthority signers
    )
    (MintTo amount)

-- | Burn tokens. Accounts: 0. `[WRITE]` account, 1. `[WRITE]` mint, then owner metas.
burn :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Instruction
burn account mint owner signers amount =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = True}
      ]
        <> ownerMetas owner signers
    )
    (Burn amount)

-- | Close a token account, reclaiming its lamports. Accounts: 0. `[WRITE]` account,
-- 1. `[WRITE]` lamport destination, then owner metas.
closeAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Instruction
closeAccount account destination owner signers =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = destination, isSigner = False, isWritable = True}
      ]
        <> ownerMetas owner signers
    )
    CloseAccount

-- | Freeze a token account. Accounts: 0. `[WRITE]` account, 1. `[]` mint, then freeze-authority metas.
freezeAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Instruction
freezeAccount account mint freezeAuthority signers =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False}
      ]
        <> ownerMetas freezeAuthority signers
    )
    FreezeAccount

-- | Thaw a frozen token account. Same accounts as 'freezeAccount'.
thawAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Instruction
thawAccount account mint freezeAuthority signers =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = account, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False}
      ]
        <> ownerMetas freezeAuthority signers
    )
    ThawAccount

-- | Transfer with decimals check. Accounts: 0. `[WRITE]` source, 1. `[]` mint,
-- 2. `[WRITE]` destination, then owner metas.
transferChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Word8 -> Instruction
transferChecked source mint destination owner signers amount decimals =
  mkInstruction
    tokenProgramId
    ( [ AccountMeta {accountPubKey = source, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = destination, isSigner = False, isWritable = True}
      ]
        <> ownerMetas owner signers
    )
    (TransferChecked amount decimals)

-- | Sync a native (wrapped SOL) account's balance. Accounts: 0. `[WRITE]` account.
syncNative :: SolanaPublicKey -> Instruction
syncNative account =
  mkInstruction
    tokenProgramId
    [AccountMeta {accountPubKey = account, isSigner = False, isWritable = True}]
    SyncNative
