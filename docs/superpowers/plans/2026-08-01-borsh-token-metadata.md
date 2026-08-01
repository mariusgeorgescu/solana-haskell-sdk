# Borsh + Token Metadata Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Minimal Borsh layer + byte-verified Token Metadata client (CreateMetadataAccountV3, UpdateMetadataAccountV2, CreateMasterEditionV3) + PDA helpers; release docs to 1.1.0.0.

**Architecture:** Borsh helpers mirror the bincode helpers' style (u32-LE lengths, u8 enum tags). Token Metadata client follows the program-client template (Stake.hs/Token.hs). Fixtures from the mpl-token-metadata crate arbitrate everything, discriminants included.

**Spec:** `docs/superpowers/specs/2026-08-01-borsh-token-metadata-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push. `-Wall` clean; hlint clean. **No new Haskell dependencies.** Fixtures additions-only. CI never runs cargo.
- Fixed constants: mint 32×12; second creator 32×33; collection key 32×34; name "Solana Haskell NFT"; symbol "SHSDK"; uri "https://example.com/nft.json"; sellerFeeBasisPoints 550; creator shares 60/40 (payer verified, 32×33 unverified); uses (Multiple, 10, 10); collectionDetails V1 size 0; maxSupply 100; update/mint authority = payer (seed 32×1).
- Program id `metaqbxxUerdq28cj1RbAWkYQm3ybzjb6a8bt518x1s`. Borsh: strings/vecs u32 LE length; Option u8 tag; enums u8 index; bool u8. Expected discriminants 33/15/17 — THE FIXTURE WINS.
- PDA seeds: metadata = ["metadata", program id raw, mint raw]; master edition = same ++ ["edition"].

---

### Task 1: Fixtures — Token Metadata vectors + PDAs

**Files:** `tools/fixture-gen/Cargo.toml` (+ mpl-token-metadata), `tools/fixture-gen/src/main.rs`; generated: `test/fixtures/metadata_instruction_data.json` (new, 6 entries), `pda.json` += 2.

- [ ] Add `mpl-token-metadata = "4"` (contingencies: "3", or hand-Borsh via `borsh` crate with the crate's types — document). Build the six instruction-data vectors named `CreateMetadataAccountV3-full`, `CreateMetadataAccountV3-minimal`, `UpdateMetadataAccountV2-some`, `UpdateMetadataAccountV2-none`, `CreateMasterEditionV3-some`, `CreateMasterEditionV3-none` using the crate's instruction builders (take `.data` from each built Instruction) with the fixed constants; derive `metadata-address`/`master-edition-address` PDAs (crate helpers `Metadata::find_pda`/`MasterEdition::find_pda` in 4.x, or find_program_address with the seed layout — document) and append to pda_fixtures. In 4.x the builder API is e.g. `CreateMetadataAccountV3Builder::new().metadata(..).mint(..).mint_authority(..).payer(..).update_authority(.., bool).data(DataV2{..}).is_mutable(true).collection_details(..).instruction()` — adapt to the resolved version, document the exact calls. Emit ALSO a 7th entry `create-v3-full-accounts`: the JSON array of the built CreateMetadataAccountV3 instruction's account metas as `[{pubkey_hex, signer, writable}]` — ground truth for the Haskell meta test.
- [ ] `cargo run`; verify: additions-only; 7 entries in the metadata file; both PDAs 32 bytes; suite still 209 green. Record: resolved crate version, the actual first byte (discriminant) of each vector, and the account-meta array. Commit `test: golden vectors for token metadata (metaplex)`.

---

### Task 2: Borsh helpers module

**Files:** Create `src/Network/Solana/Core/Borsh.hs`, `test/Test/Core/Borsh.hs`; cabal + test/Main.hs.

**Interfaces (produces):** `putBorshString :: String -> Put`, `getBorshString :: Get String` (u32 LE byte-length + UTF-8; get: length guard vs maxBound Int + decodeUtf8' fail — mirror getBincodeString exactly except u32), `putBorshOption :: (a -> Put) -> Maybe a -> Put`, `getBorshOption :: Get a -> Get (Maybe a)` (u8 tag, fail on >1), `putBorshVec :: (a -> Put) -> [a] -> Put`, `getBorshVec :: Get a -> Get [a]` (u32 LE count + elements, count guard), `putBorshBool :: Bool -> Put`, `getBorshBool :: Get Bool` (u8 0/1, fail otherwise).

- [ ] TDD: unit tests pinning exact bytes — `putBorshString "ab"` = `02 00 00 00 61 62` (u32 not u64 — THE distinguishing test), option tags, vec counts, bool 0/1 + invalid-bool decode failure, string/vec/option round-trip properties (ASCII strings; small lists) → module (Haddock noting the borsh-vs-bincode differences) → green (209 + ~8). Commit `feat: borsh serialization helpers`.

---

### Task 3: Token Metadata client

**Files:** Create `src/Network/Solana/Metaplex/TokenMetadata.hs`, `test/Test/Metaplex/TokenMetadata.hs`; cabal + test/Main.hs.

**Interfaces (produces):** program id; `deriveMetadataAddress`/`deriveMasterEditionAddress :: SolanaPublicKey -> Maybe SolanaPublicKey`; support types + `TokenMetadataInstruction` per the spec (Borsh field order: DataV2 = name, symbol, uri, sellerFeeBasisPoints u16 LE, creators Option<Vec<Creator>>, collection Option<Collection>, uses Option<Uses>; Creator = address, verified, share; Collection = verified, key; Uses = method, remaining, total; CreateMetadataAccountV3 = disc + DataV2 + isMutable + Option<CollectionDetails>; UpdateMetadataAccountV2 = disc + Option<DataV2> + Option<Pubkey> + Option<bool> + Option<bool>; CreateMasterEditionV3 = disc + Option<u64>); builders:

```haskell
createMetadataAccountV3 :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Bool -> DataV2 -> Bool -> Maybe CollectionDetails -> Instruction
-- mint, mintAuthority, payer, updateAuthority, updateAuthorityIsSigner, data, isMutable, collectionDetails
-- metas: [w metadata(derived), ro mint, s mintAuthority, ws payer, updateAuthority (ro; signer = updateAuthorityIsSigner), ro SP.systemProgramId] ++ rent? — MATCH the create-v3-full-accounts fixture EXACTLY (it is ground truth; adjust this list to it and document)
updateMetadataAccountV2 :: SolanaPublicKey -> SolanaPublicKey -> Maybe DataV2 -> Maybe SolanaPublicKey -> Maybe Bool -> Maybe Bool -> Instruction
-- mint, updateAuthority, args...: metas [w metadata(derived), s updateAuthority]
createMasterEditionV3 :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Maybe Word64 -> Instruction
-- mint, updateAuthority, mintAuthority, payer, maxSupply: metas [w edition(derived), w mint, s updateAuthority, s mintAuthority, ws payer, ro metadata(derived), ro Token.tokenProgramId, ro SP.systemProgramId, ro Sysvar.rent]
```

(PDA-derivation failure: `error`, documented — established precedent.)

- [ ] TDD: tests — 2 PDA goldens; 6 data goldens (constants above; payer as first creator verified/60, 32×33 unverified/40); the createMetadataAccountV3 meta test asserts against the `create-v3-full-accounts` fixture (decode its JSON in the test via Test.Fixtures-style loading — extend the Fixture loader ONLY if needed; a small local Aeson decode of the array file entry is fine); meta tests for the other two builders (hand-specified per the interfaces, cross-checked in review); round-trip property over all instruction variants (small generators); unknown-discriminant test (byte 200) → register (Metaplex namespace after Core/NativePrograms — alphabetical: Metaplex.TokenMetadata between Core.* and NativePrograms.*? cabal list is alphabetical: Network.Solana.Constants, Core.*, Metaplex.TokenMetadata, NativePrograms.* — verify and place) → implement → green (expect +~13). Commit `feat: metaplex token metadata client (create/update metadata, master edition)`.

---

### Task 4: Docs + release version

**Files:** README.md, CHANGELOG.md, solana-haskell-sdk.cabal.

- [ ] README: Features gains a `- [x] Metaplex Token Metadata (create/update metadata, master edition)` under a new `### Metaplex` grouping (or as an SPL-sibling top-level — match the list's structure); REMOVE the Post-1.0 section entirely (both items now shipped; move any remaining future items — none — or delete the heading). CHANGELOG: fold `## Unreleased` into a new `## 1.1.0.0 -- 2026-08-01` section, adding `* Added Metaplex Token Metadata client (CreateMetadataAccountV3, UpdateMetadataAccountV2, CreateMasterEditionV3) with metadata/master-edition PDA derivation, built on a new minimal Borsh serialization layer.` cabal: `version: 1.1.0.0`.
- [ ] Suite green; `cabal haddock` exit 0; the SDIST GATE re-run (sdist → unpack → build+test from tarball — the fixture glob must pick up the new JSON; expect all tests green). Commit `release: 1.1.0.0 — metaplex token metadata, v0 transactions, address lookup tables`.

## Success criteria

Spec's four criteria; sdist gate re-verified for 1.1.0.0.
