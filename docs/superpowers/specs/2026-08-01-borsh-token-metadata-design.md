# Borsh + Metaplex Token Metadata — Design

**Date:** 2026-08-01
**Status:** Approved (post-1.0 sub-project B; autonomous-goal mode)
**Goal:** A minimal Borsh serialization layer and a byte-verified Metaplex Token Metadata client — the last post-1.0 roadmap item.

## Context

Metaplex programs serialize instruction args with Borsh, not bincode. Key differences: strings and vectors carry a **u32** LE length (bincode: u64); enums use a **u8** variant index (bincode: u32); Option is a u8 tag (same). The SDK (209 tests) has everything else needed. Ground truth: the `mpl-token-metadata` Rust crate via fixture-gen. Work on `development`; never push.

## Scope

### Network.Solana.Core.Borsh (new)

Put/Get helper pairs (no typeclass machinery — direct composition like the rest of the SDK): `putBorshString`/`getBorshString` (u32 LE byte-length + UTF-8; get validates UTF-8 and guards the length like `getBincodeString`), `putBorshOption`/`getBorshOption` (u8 tag 0/1), `putBorshVec`/`getBorshVec` (u32 LE count), plus doc-notes that u8/u16/u32/u64/bool map to the existing `Data.Binary.Put` LE primitives (bool = u8 0/1: `putBorshBool`/`getBorshBool`).

### Network.Solana.Metaplex.TokenMetadata (new namespace)

- `tokenMetadataProgramId = "metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s"`.
- PDA helpers: `deriveMetadataAddress :: SolanaPublicKey -> Maybe SolanaPublicKey` (seeds ["metadata", program id, mint]) and `deriveMasterEditionAddress :: SolanaPublicKey -> Maybe SolanaPublicKey` (same + trailing "edition").
- Support types (Borsh, field order per the Rust crate): `Creator {crAddress :: SolanaPublicKey, crVerified :: Bool, crShare :: Word8}`; `Collection {colVerified :: Bool, colKey :: SolanaPublicKey}`; `UseMethod = Burn | Multiple | Single` (u8 0/1/2); `Uses {usesUseMethod :: UseMethod, usesRemaining :: Word64, usesTotal :: Word64}`; `CollectionDetails = CollectionDetailsV1 {cdSize :: Word64}` (enum u8 0 + u64); `DataV2 {dName, dSymbol, dUri :: String, dSellerFeeBasisPoints :: Word16, dCreators :: Maybe [Creator], dCollection :: Maybe Collection, dUses :: Maybe Uses}`.
- Instructions (u8 Borsh discriminants; THE FIXTURE ARBITRATES the numbers): `CreateMetadataAccountV3 {cmaData :: DataV2, cmaIsMutable :: Bool, cmaCollectionDetails :: Maybe CollectionDetails}` (expected 33), `UpdateMetadataAccountV2 {umaData :: Maybe DataV2, umaUpdateAuthority :: Maybe SolanaPublicKey, umaPrimarySaleHappened :: Maybe Bool, umaIsMutable :: Maybe Bool}` (expected 15), `CreateMasterEditionV3 {cmeMaxSupply :: Maybe Word64}` (expected 17). Symmetric Binary via the Borsh helpers.
- Builders (metas per the Rust crate's account lists):
  - `createMetadataAccountV3 mint mintAuthority payer updateAuthority updateAuthorityIsSigner dataV2 isMutable collectionDetails` → metas [w metadata(PDA), ro mint, s mintAuthority, ws payer, ro(or s) updateAuthority, ro system, ro rent? — v3 uses OPTIONAL rent: the crate omits rent; fixture/crate arbitrates].
  - `updateMetadataAccountV2 mint updateAuthority args` → metas [w metadata, s updateAuthority].
  - `createMasterEditionV3 mint updateAuthority mintAuthority payer maxSupply` → metas [w edition(PDA), w mint, s updateAuthority, s mintAuthority, ws payer, ro metadata, ro tokenProgramId, ro system, ro rent — per crate].
- The deferred rest of the program (50+ instructions, pNFTs, delegates) stays out of scope, Haddock-noted.

## Testing

fixture-gen gains `mpl-token-metadata = "4"` (generator-only; if it cannot resolve against solana-sdk 1.18, fall back to "3" or hand-Borsh via the `borsh` crate with the crate's arg structs — document the path). Vectors:
- `metadata_instruction_data.json`: CreateMetadataAccountV3 (full: 2 creators, collection, uses, collection-details) and (minimal: all Nones), UpdateMetadataAccountV2 (all-Some and all-None), CreateMasterEditionV3 (Some 100 and None). Constants: name "Solana Haskell NFT", symbol "SHSDK", uri "https://example.com/nft.json", sellerFee 550, creators [payer verified share 60, 32×33 unverified share 40], collection key 32×34, uses (Multiple, 10, 10), collectionDetails size 0? (V1 {size: 0}), maxSupply 100.
- `pda.json` += `metadata-address`, `master-edition-address` (mint 32×12).
- Haskell: goldens per variant; PDA goldens; meta tests per builder; round-trip property; unknown-discriminant test; Borsh helper unit tests (string length prefix is u32 not u64 — pin with an explicit byte test).

## Out of scope

All other Token Metadata instructions; pNFT/programmable configs; Metaplex Core/Bubblegum; on-chain account-state (Metadata struct) parsing; a general Borsh typeclass.

## Success criteria

1. Suite green. 2. All six instruction goldens + two PDA goldens byte-identical to the mpl crate. 3. README/CHANGELOG updated (Metaplex leaves Post-1.0; version bumped to 1.1.0.0 with the Unreleased section folded in). 4. Final review clean.
