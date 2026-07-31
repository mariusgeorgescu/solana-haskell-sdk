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
