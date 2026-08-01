{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Metaplex Token Metadata program: attaches on-chain
-- metadata (name, symbol, URI, creators, royalties) to SPL Token mints and
-- registers "master edition" NFTs. Instruction data is Borsh-encoded (see
-- "Network.Solana.Core.Borsh"), unlike the native and SPL programs covered
-- elsewhere in this SDK, which use bincode or hand-rolled formats.
--
-- Only three instructions are modeled: 'createMetadataAccountV3',
-- 'updateMetadataAccountV2' and 'createMasterEditionV3'. The rest of the
-- program (50+ instructions, programmable NFTs, delegates, Token-2022
-- support) is out of scope.
module Network.Solana.Metaplex.TokenMetadata
  ( tokenMetadataProgramId,
    deriveMetadataAddress,
    deriveMasterEditionAddress,
    UseMethod (..),
    Creator (..),
    Collection (..),
    Uses (..),
    CollectionDetails (..),
    DataV2 (..),
    TokenMetadataInstruction (..),
    createMetadataAccountV3,
    updateMetadataAccountV2,
    createMasterEditionV3,
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.Solana.Core.Borsh
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Pda (findProgramAddress)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.SplPrograms.Token qualified as Token
import Network.Solana.Sysvar qualified as Sysvar

-- | Metaplex Token Metadata program address.
tokenMetadataProgramId :: SolanaPublicKey
tokenMetadataProgramId = "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s"

-- | Derive the metadata PDA for a mint: the PDA of
-- @["metadata", program id, mint]@ under the Token Metadata program.
deriveMetadataAddress :: SolanaPublicKey -> Maybe SolanaPublicKey
deriveMetadataAddress mint =
  fst
    <$> findProgramAddress
      [ "metadata",
        getSolanaPublicKeyRaw tokenMetadataProgramId,
        getSolanaPublicKeyRaw mint
      ]
      tokenMetadataProgramId

-- | Derive the master edition PDA for a mint: the PDA of
-- @["metadata", program id, mint, "edition"]@ under the Token Metadata program.
deriveMasterEditionAddress :: SolanaPublicKey -> Maybe SolanaPublicKey
deriveMasterEditionAddress mint =
  fst
    <$> findProgramAddress
      [ "metadata",
        getSolanaPublicKeyRaw tokenMetadataProgramId,
        getSolanaPublicKeyRaw mint,
        "edition"
      ]
      tokenMetadataProgramId

-- | Which "uses" behavior an NFT carries (Borsh u8 enum index 0\/1\/2).
data UseMethod = Burn | Multiple | Single
  deriving (Eq, Show, Enum, Bounded, Generic)

putUseMethod :: UseMethod -> Put
putUseMethod = putWord8 . fromIntegral . fromEnum

getUseMethod :: Get UseMethod
getUseMethod = do
  b <- getWord8
  if b <= 2
    then pure (toEnum (fromIntegral b))
    else fail ("UseMethod: invalid value " <> show b)

-- | A single NFT creator entry: address, whether they have verified their
-- inclusion, and their royalty share (0-100).
data Creator = Creator
  { crAddress :: SolanaPublicKey,
    crVerified :: Bool,
    crShare :: Word8
  }
  deriving (Eq, Show, Generic)

instance Binary Creator where
  put :: Creator -> Put
  put (Creator addr verified share) = do
    putByteString (getSolanaPublicKeyRaw addr)
    putBorshBool verified
    putWord8 share
  get :: Get Creator
  get = Creator <$> get <*> getBorshBool <*> getWord8

-- | The collection an NFT belongs to.
data Collection = Collection
  { colVerified :: Bool,
    colKey :: SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

instance Binary Collection where
  put :: Collection -> Put
  put (Collection verified key) = do
    putBorshBool verified
    putByteString (getSolanaPublicKeyRaw key)
  get :: Get Collection
  get = Collection <$> getBorshBool <*> get

-- | Limited-use configuration for an NFT (e.g. burn-on-use tickets).
data Uses = Uses
  { usesUseMethod :: UseMethod,
    usesRemaining :: Word64,
    usesTotal :: Word64
  }
  deriving (Eq, Show, Generic)

instance Binary Uses where
  put :: Uses -> Put
  put (Uses method usesRemaining' total) = do
    putUseMethod method
    putWord64le usesRemaining'
    putWord64le total
  get :: Get Uses
  get = Uses <$> getUseMethod <*> getWord64le <*> getWord64le

-- | Marks a Metadata account as a collection ("sized" collection, V1). Only
-- the V1 variant is modeled; decoding any other variant tag fails.
newtype CollectionDetails = CollectionDetailsV1 {cdSize :: Word64}
  deriving (Eq, Show, Generic)

instance Binary CollectionDetails where
  put :: CollectionDetails -> Put
  put (CollectionDetailsV1 size) = do
    putWord8 0
    putWord64le size
  get :: Get CollectionDetails
  get = do
    variant <- getWord8
    case variant of
      0 -> CollectionDetailsV1 <$> getWord64le
      _ -> fail ("CollectionDetails: unsupported variant " <> show variant)

-- | The mutable on-chain fields of a Metadata account.
data DataV2 = DataV2
  { dName :: String,
    dSymbol :: String,
    dUri :: String,
    dSellerFeeBasisPoints :: Word16,
    dCreators :: Maybe [Creator],
    dCollection :: Maybe Collection,
    dUses :: Maybe Uses
  }
  deriving (Eq, Show, Generic)

instance Binary DataV2 where
  put :: DataV2 -> Put
  put (DataV2 name symbol uri fee creators collection uses) = do
    putBorshString name
    putBorshString symbol
    putBorshString uri
    putWord16le fee
    putBorshOption (putBorshVec put) creators
    putBorshOption put collection
    putBorshOption put uses
  get :: Get DataV2
  get =
    DataV2
      <$> getBorshString
      <*> getBorshString
      <*> getBorshString
      <*> getWord16le
      <*> getBorshOption (getBorshVec get)
      <*> getBorshOption get
      <*> getBorshOption get

-- | Token Metadata instructions covered by this client (Borsh u8
-- discriminants, arbitrated by the @mpl-token-metadata@ crate's fixtures).
data TokenMetadataInstruction
  = -- | Create a Metadata account (v3) for a mint. Discriminant 33.
    CreateMetadataAccountV3
      { cmaData :: DataV2,
        cmaIsMutable :: Bool,
        cmaCollectionDetails :: Maybe CollectionDetails
      }
  | -- | Update an existing Metadata account (v2). Discriminant 15.
    UpdateMetadataAccountV2
      { umaData :: Maybe DataV2,
        umaUpdateAuthority :: Maybe SolanaPublicKey,
        umaPrimarySaleHappened :: Maybe Bool,
        umaIsMutable :: Maybe Bool
      }
  | -- | Register a Metadata account as a Master Edition (v3). Discriminant 17.
    CreateMasterEditionV3
      { cmeMaxSupply :: Maybe Word64
      }
  deriving (Eq, Show, Generic)

instance Binary TokenMetadataInstruction where
  put :: TokenMetadataInstruction -> Put
  put (CreateMetadataAccountV3 dataV2 isMutable collectionDetails) = do
    putWord8 33
    put dataV2
    putBorshBool isMutable
    putBorshOption put collectionDetails
  put (UpdateMetadataAccountV2 dataV2 updateAuthority primarySaleHappened isMutable) = do
    putWord8 15
    putBorshOption put dataV2
    putBorshOption (putByteString . getSolanaPublicKeyRaw) updateAuthority
    putBorshOption putBorshBool primarySaleHappened
    putBorshOption putBorshBool isMutable
  put (CreateMasterEditionV3 maxSupply) = do
    putWord8 17
    putBorshOption putWord64le maxSupply

  get :: Get TokenMetadataInstruction
  get = do
    disc <- getWord8
    case disc of
      33 -> CreateMetadataAccountV3 <$> get <*> getBorshBool <*> getBorshOption get
      15 ->
        UpdateMetadataAccountV2
          <$> getBorshOption get
          <*> getBorshOption get
          <*> getBorshOption getBorshBool
          <*> getBorshOption getBorshBool
      17 -> CreateMasterEditionV3 <$> getBorshOption getWord64le
      _ -> fail ("TokenMetadataInstruction: unknown discriminant " <> show disc)

-- | Create a new Metadata account (v3) for a mint.
-- Fails with 'error' if PDA derivation fails (established precedent, see
-- "Network.Solana.SplPrograms.AssociatedTokenAccount").
-- # Account references (no rent account — matches the reference crate,
-- which omits the optional rent account entirely when unset)
-- 0. `[WRITE]` Metadata account (derived)
-- 1. `[]` Mint account
-- 2. `[SIGNER]` Mint authority
-- 3. `[WRITE, SIGNER]` Payer
-- 4. `[]` Update authority (signer iff @updateAuthorityIsSigner@)
-- 5. `[]` System program
createMetadataAccountV3 :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Bool -> DataV2 -> Bool -> Maybe CollectionDetails -> Instruction
createMetadataAccountV3 mint mintAuthority payer updateAuthority updateAuthorityIsSigner dataV2 isMutable collectionDetails =
  mkInstruction
    tokenMetadataProgramId
    [ AccountMeta {accountPubKey = metadata, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = mintAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True},
      AccountMeta {accountPubKey = updateAuthority, isSigner = updateAuthorityIsSigner, isWritable = False},
      AccountMeta {accountPubKey = SystemProgram.systemProgramId, isSigner = False, isWritable = False}
    ]
    (CreateMetadataAccountV3 dataV2 isMutable collectionDetails)
  where
    metadata = fromMaybe (error "createMetadataAccountV3: metadata PDA derivation failed") (deriveMetadataAddress mint)

-- | Update an existing Metadata account (v2).
-- # Account references
-- 0. `[WRITE]` Metadata account (derived)
-- 1. `[SIGNER]` Update authority
updateMetadataAccountV2 :: SolanaPublicKey -> SolanaPublicKey -> Maybe DataV2 -> Maybe SolanaPublicKey -> Maybe Bool -> Maybe Bool -> Instruction
updateMetadataAccountV2 mint updateAuthority dataV2 newUpdateAuthority primarySaleHappened isMutable =
  mkInstruction
    tokenMetadataProgramId
    [ AccountMeta {accountPubKey = metadata, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = updateAuthority, isSigner = True, isWritable = False}
    ]
    (UpdateMetadataAccountV2 dataV2 newUpdateAuthority primarySaleHappened isMutable)
  where
    metadata = fromMaybe (error "updateMetadataAccountV2: metadata PDA derivation failed") (deriveMetadataAddress mint)

-- | Register a Metadata account as a Master Edition (v3), fixing the
-- maximum number of print editions (or making it unlimited when 'Nothing').
-- Fails with 'error' if PDA derivation fails (see 'createMetadataAccountV3').
-- # Account references
-- 0. `[WRITE]` Master Edition account (derived)
-- 1. `[WRITE]` Mint account
-- 2. `[SIGNER]` Update authority
-- 3. `[SIGNER]` Mint authority
-- 4. `[WRITE, SIGNER]` Payer
-- 5. `[]` Metadata account (derived)
-- 6. `[]` Token program
-- 7. `[]` System program
-- 8. `[]` Rent sysvar
createMasterEditionV3 :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Maybe Word64 -> Instruction
createMasterEditionV3 mint updateAuthority mintAuthority payer maxSupply =
  mkInstruction
    tokenMetadataProgramId
    [ AccountMeta {accountPubKey = edition, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = mint, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = updateAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = mintAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True},
      AccountMeta {accountPubKey = metadata, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Token.tokenProgramId, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = SystemProgram.systemProgramId, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False}
    ]
    (CreateMasterEditionV3 maxSupply)
  where
    edition = fromMaybe (error "createMasterEditionV3: master edition PDA derivation failed") (deriveMasterEditionAddress mint)
    metadata = fromMaybe (error "createMasterEditionV3: metadata PDA derivation failed") (deriveMetadataAddress mint)
