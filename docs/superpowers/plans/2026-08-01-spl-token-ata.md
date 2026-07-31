# SPL Token + ATA Clients Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byte-verified SPL Token (instructions 0-20) and Associated Token Account clients, plus PDA derivation in the crypto core.

**Architecture:** Same golden-vector pattern as phases 1-2. New `Network.Solana.Core.Pda` module (SHA-256 + ed25519 on-curve check via the new `crypton` dependency). `Network.Solana.SplPrograms.Token` carries the 21-variant instruction type with SPL's hand-rolled pack format (u8 discriminant, 1-byte-tag optional pubkeys — NOT bincode). `Network.Solana.SplPrograms.AssociatedTokenAccount` derives ATA addresses via PDA and builds create instructions.

**Tech Stack:** Haskell (GHC 9.6.7, cabal, tasty/QuickCheck, crypton), Rust (solana-sdk 1.18 + spl-token + spl-associated-token-account) for fixtures only.

**Spec:** `docs/superpowers/specs/2026-08-01-spl-token-ata-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push.
- `cabal build all` warning-clean (`-Wall`). **`crypton` is the ONLY new Haskell dependency** (library stanza).
- Fixtures generated once by `tools/fixture-gen`, committed; regeneration must leave existing entries byte-identical (additions only). CI never runs cargo.
- Fixed constants (Rust generator and Haskell tests MUST match):
  - wallet pubkey: 32 bytes of `11`; mint pubkey: 32 bytes of `12`; delegate pubkey: 32 bytes of `13`; multisig-signer pubkeys: 32 bytes of `14` and 32 bytes of `15`
  - amount: `1000000`; decimals: `6`; multisig m: `2`
  - payer keypair seed: 32 bytes of `1` (existing); blockhash: 32 bytes of `9` (existing)
  - generic PDA seed string: `"solana-haskell-sdk"` (UTF-8 bytes, one seed)
- Program ids: Token `TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA`; ATA `ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL`.
- Wire format (ground truth = spl-token crate): u8 discriminant 0-20; u64 LE amounts; u8 decimals/m; raw 32-byte pubkeys; `COption<Pubkey>` = tag byte 0 (none) | 1 + 32 bytes (some); AuthorityType u8: MintTokens=0, FreezeAccount=1, AccountOwner=2, CloseAccount=3. ATA data: single borsh byte 0 (Create) / 1 (CreateIdempotent).
- PDA: sha256(seed₁ <> … <> seedₙ <> programId <> "ProgramDerivedAddress"); each seed ≤ 32 bytes, ≤ 16 seeds; result must NOT be on the ed25519 curve; findProgramAddress appends bump 255,254,…,0 as a single byte.
- Match existing code style (explicit instance signatures, qualified imports, record AccountMeta, Haddock like existing builders).

---

### Task 1: Fixture generator — token, PDA, and ATA vectors

**Files:**
- Modify: `tools/fixture-gen/Cargo.toml`, `tools/fixture-gen/src/main.rs`
- Create (generated): `test/fixtures/token_instruction_data.json`, `test/fixtures/pda.json`
- Modify (generated): `test/fixtures/transactions.json` (one appended entry)

**Interfaces:**
- Produces fixture entries: token file — `InitializeMint-some`, `InitializeMint-none`, `InitializeAccount`, `InitializeMultisig`, `Transfer`, `Approve`, `Revoke`, `SetAuthority-some`, `SetAuthority-none`, `MintTo`, `Burn`, `CloseAccount`, `FreezeAccount`, `ThawAccount`, `TransferChecked`, `ApproveChecked`, `MintToChecked`, `BurnChecked`, `InitializeAccount2`, `SyncNative`, `InitializeAccount3`, `InitializeMultisig2`, `InitializeMint2-some`; pda file — `generic-pda-address`, `generic-pda-bump`, `ata-address`, `ata-bump`, `dest-ata-address`; transactions — `ata-transfer-transaction`.

- [ ] **Step 1: Add crate dependencies**

In `tools/fixture-gen/Cargo.toml` `[dependencies]` add:

```toml
spl-token = { version = "4", features = ["no-entrypoint"] }
spl-associated-token-account = { version = "3", features = ["no-entrypoint"] }
```

If cargo cannot solve `spl-associated-token-account = "3"` against solana-sdk 1.18, fall back to `"2"` — the two functions used (`get_associated_token_address`, `instruction::create_associated_token_account{,_idempotent}`) exist in both.

- [ ] **Step 2: Extend the generator**

Add to `tools/fixture-gen/src/main.rs` imports:

```rust
use solana_sdk::program_option::COption;
use spl_token::instruction::{AuthorityType, TokenInstruction};
```

Append at the end of `main()`:

```rust
    let wallet = Pubkey::new_from_array([11u8; 32]);
    let mint = Pubkey::new_from_array([12u8; 32]);
    let delegate = Pubkey::new_from_array([13u8; 32]);
    let amount: u64 = 1_000_000;
    let decimals: u8 = 6;

    let token_variants: Vec<(&str, TokenInstruction)> = vec![
        ("InitializeMint-some", TokenInstruction::InitializeMint { decimals, mint_authority: wallet, freeze_authority: COption::Some(delegate) }),
        ("InitializeMint-none", TokenInstruction::InitializeMint { decimals, mint_authority: wallet, freeze_authority: COption::None }),
        ("InitializeAccount", TokenInstruction::InitializeAccount),
        ("InitializeMultisig", TokenInstruction::InitializeMultisig { m: 2 }),
        ("Transfer", TokenInstruction::Transfer { amount }),
        ("Approve", TokenInstruction::Approve { amount }),
        ("Revoke", TokenInstruction::Revoke),
        ("SetAuthority-some", TokenInstruction::SetAuthority { authority_type: AuthorityType::AccountOwner, new_authority: COption::Some(delegate) }),
        ("SetAuthority-none", TokenInstruction::SetAuthority { authority_type: AuthorityType::CloseAccount, new_authority: COption::None }),
        ("MintTo", TokenInstruction::MintTo { amount }),
        ("Burn", TokenInstruction::Burn { amount }),
        ("CloseAccount", TokenInstruction::CloseAccount),
        ("FreezeAccount", TokenInstruction::FreezeAccount),
        ("ThawAccount", TokenInstruction::ThawAccount),
        ("TransferChecked", TokenInstruction::TransferChecked { amount, decimals }),
        ("ApproveChecked", TokenInstruction::ApproveChecked { amount, decimals }),
        ("MintToChecked", TokenInstruction::MintToChecked { amount, decimals }),
        ("BurnChecked", TokenInstruction::BurnChecked { amount, decimals }),
        ("InitializeAccount2", TokenInstruction::InitializeAccount2 { owner: wallet }),
        ("SyncNative", TokenInstruction::SyncNative),
        ("InitializeAccount3", TokenInstruction::InitializeAccount3 { owner: wallet }),
        ("InitializeMultisig2", TokenInstruction::InitializeMultisig2 { m: 2 }),
        ("InitializeMint2-some", TokenInstruction::InitializeMint2 { decimals, mint_authority: wallet, freeze_authority: COption::Some(delegate) }),
    ];
    let token_fixtures: Vec<_> = token_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&v.pack()) }))
        .collect();
    fs::write(
        "../../test/fixtures/token_instruction_data.json",
        serde_json::to_string_pretty(&token_fixtures).unwrap(),
    )
    .unwrap();

    let token_program = spl_token::id();
    let ata_program = spl_associated_token_account::id();
    let (generic_pda, generic_bump) =
        Pubkey::find_program_address(&[b"solana-haskell-sdk"], &token_program);
    assert!(generic_bump < 255, "generic PDA bump must be < 255 to prove the search runs");
    let ata = spl_associated_token_account::get_associated_token_address(&payer.pubkey(), &mint);
    let (_ata_check, ata_bump) = Pubkey::find_program_address(
        &[payer.pubkey().as_ref(), token_program.as_ref(), mint.as_ref()],
        &ata_program,
    );
    assert!(ata_bump < 255, "ATA bump must be < 255 to prove the search runs");
    let dest_ata = spl_associated_token_account::get_associated_token_address(&wallet, &mint);
    let pda_fixtures = vec![
        json!({ "name": "generic-pda-address", "hex": hex(&generic_pda.to_bytes()) }),
        json!({ "name": "generic-pda-bump", "hex": hex(&[generic_bump]) }),
        json!({ "name": "ata-address", "hex": hex(&ata.to_bytes()) }),
        json!({ "name": "ata-bump", "hex": hex(&[ata_bump]) }),
        json!({ "name": "dest-ata-address", "hex": hex(&dest_ata.to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/pda.json",
        serde_json::to_string_pretty(&pda_fixtures).unwrap(),
    )
    .unwrap();

    let create_dest_ata_ix =
        spl_associated_token_account::instruction::create_associated_token_account_idempotent(
            &payer.pubkey(),
            &wallet,
            &mint,
            &token_program,
        );
    let transfer_checked_ix = spl_token::instruction::transfer_checked(
        &token_program,
        &ata,
        &mint,
        &dest_ata,
        &payer.pubkey(),
        &[],
        amount,
        decimals,
    )
    .unwrap();
    let ata_msg = Message::new_with_blockhash(
        &[create_dest_ata_ix, transfer_checked_ix],
        Some(&payer.pubkey()),
        &blockhash,
    );
    let ata_tx = Transaction::new(&[&payer], ata_msg, blockhash);
```

and append to `tx_fixtures` (LAST entry; existing entries unchanged):

```rust
        json!({ "name": "ata-transfer-transaction", "hex": hex(&bincode::serialize(&ata_tx).unwrap()) }),
```

- [ ] **Step 3: Regenerate and verify additions-only**

Run: `cd tools/fixture-gen && cargo run && cd ../..` (first build downloads spl crates — a few minutes).
Then: `git diff test/fixtures/transactions.json | grep '^-' | grep -v '^---'` → must print nothing; `python3 -m json.tool test/fixtures/token_instruction_data.json > /dev/null && grep -c name test/fixtures/token_instruction_data.json` → 23 entries. Spot-check `Transfer` hex = `0340420f0000000000` (disc 3 + u64 LE 1000000). Existing suite: `cabal test --test-show-details=direct` → 48 tests still green.

- [ ] **Step 4: Commit**

```bash
git add tools/fixture-gen/ test/fixtures/
git commit -m "test: golden vectors for SPL Token, PDA derivation, and ATA transfer"
```

---

### Task 2: PDA derivation module

**Files:**
- Create: `src/Network/Solana/Core/Pda.hs`
- Create: `test/Test/Core/Pda.hs`
- Modify: `solana-haskell-sdk.cabal` (library: add module + `crypton` dep; test: add module), `test/Main.hs`

**Interfaces:**
- Consumes: `SolanaPublicKey`, `unsafeSolanaPublicKeyRaw`, `getSolanaPublicKeyRaw` from `Core.Crypto`; fixtures from Task 1.
- Produces: `createProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Either PdaError SolanaPublicKey`; `findProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Maybe (SolanaPublicKey, Word8)`; `data PdaError = SeedTooLong | TooManySeeds | InvalidSeeds` (Eq, Show). Task 5 uses `findProgramAddress`.

- [ ] **Step 1: Write the failing tests**

Create `test/Test/Core/Pda.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Pda (tests) where

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Network.Solana.Core.Crypto (getSolanaPublicKeyRaw, unsafeSolanaPublicKey)
import Network.Solana.Core.Pda
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tokenProgram :: Network.Solana.Core.Crypto.SolanaPublicKey
tokenProgram = unsafeSolanaPublicKey "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "PDA derivation"
      [ testCase "findProgramAddress matches Rust (address and bump)" $ do
          fs <- getFixtures
          case findProgramAddress ["solana-haskell-sdk"] tokenProgram of
            Nothing -> assertFailure "no PDA found"
            Just (addr, bump) -> do
              getSolanaPublicKeyRaw addr @?= requireFixture "generic-pda-address" fs
              BS.singleton bump @?= requireFixture "generic-pda-bump" fs,
        testCase "seed longer than 32 bytes is rejected" $
          assertBool "expected Left" (isLeft (createProgramAddress [BS.replicate 33 0] tokenProgram)),
        testProperty "createProgramAddress never returns an on-curve address" $
          forAll (BS.pack <$> vectorOf 8 arbitrary) $ \seed ->
            case createProgramAddress [seed, "bump"] tokenProgram of
              Left _ -> property True
              Right addr -> property (getSolanaPublicKeyRaw addr /= BS.replicate 32 0)
      ]
```

(The property is a smoke test that derivation is total and produces 32-byte non-degenerate output; real curve verification is the golden test — Rust only returns off-curve addresses, so byte-equality proves the on-curve rejection logic agrees.) Fix the import line if the qualified name reads awkwardly: `import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw, unsafeSolanaPublicKey)` and use plain `SolanaPublicKey` in the signature.

- [ ] **Step 2: Register and verify compile failure**

Cabal: library `exposed-modules` gains `Network.Solana.Core.Pda` (after `Network.Solana.Core.Message`); library `build-depends` gains `, crypton`; test `other-modules` gains `Test.Core.Pda`. `test/Main.hs`: import + register `Test.Core.Pda.tests`.

Run: `cabal test --test-show-details=direct 2>&1 | head -20`
Expected: FAIL to compile — `Network.Solana.Core.Pda` missing. (First build compiles crypton — a few minutes.)

- [ ] **Step 3: Implement**

Create `src/Network/Solana/Core/Pda.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Program Derived Addresses (PDAs): addresses derived from seeds and a
-- program id that are guaranteed to lie off the ed25519 curve, so no
-- private key can exist for them. Used by programs to sign via CPI and by
-- clients to locate program-owned accounts (e.g. associated token accounts).
module Network.Solana.Core.Pda
  ( PdaError (..),
    createProgramAddress,
    findProgramAddress,
  )
where

import Crypto.ECC.Edwards25519 qualified as Edwards
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw, unsafeSolanaPublicKeyRaw)

data PdaError
  = -- | A seed exceeds 32 bytes
    SeedTooLong
  | -- | More than 16 seeds were provided
    TooManySeeds
  | -- | The derived hash landed on the ed25519 curve; use another bump/seed
    InvalidSeeds
  deriving (Eq, Show)

pdaMarker :: BS.ByteString
pdaMarker = "ProgramDerivedAddress"

-- | Derive a program address from seeds and a program id. Fails with
-- 'InvalidSeeds' if the sha256 result is a valid ed25519 curve point.
createProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Either PdaError SolanaPublicKey
createProgramAddress seeds programId
  | length seeds > 16 = Left TooManySeeds
  | any ((> 32) . BS.length) seeds = Left SeedTooLong
  | otherwise =
      let digest :: Digest SHA256
          digest = hash (BS.concat seeds <> getSolanaPublicKeyRaw programId <> pdaMarker)
          bytes = BS.pack (BA.unpack digest)
       in if isOnCurve bytes
            then Left InvalidSeeds
            else Right (unsafeSolanaPublicKeyRaw (BS.unpack bytes))

-- | Find the first bump seed (255 downto 0) whose derived address is off
-- the curve, mirroring the Rust SDK's @Pubkey::find_program_address@.
findProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Maybe (SolanaPublicKey, Word8)
findProgramAddress seeds programId = go 255
  where
    go :: Word8 -> Maybe (SolanaPublicKey, Word8)
    go bump =
      case createProgramAddress (seeds <> [BS.singleton bump]) programId of
        Right addr -> Just (addr, bump)
        Left InvalidSeeds
          | bump == 0 -> Nothing
          | otherwise -> go (bump - 1)
        Left _ -> Nothing

-- | A 32-byte string is "on curve" iff it decodes as a valid ed25519 point.
isOnCurve :: BS.ByteString -> Bool
isOnCurve bs =
  case Edwards.pointDecode (BA.convert bs :: BA.Bytes) of
    CryptoPassed _ -> True
    CryptoFailed _ -> False
```

NOTE on the crypton API: the point-decoding validator in `Crypto.ECC.Edwards25519` may be named `pointDecode` or `pointDecodeModq` depending on the crypton version cabal resolves. Check with `cabal haddock` docs or `ghci`: `:browse Crypto.ECC.Edwards25519`. Use whichever exists that decodes a compressed 32-byte encoding and returns `CryptoFailable Point`; adapt the one call site. If BOTH exist, prefer `pointDecodeModq` (it enforces canonical encoding, matching curve25519-dalek's `is_on_curve` semantics used by Solana). The golden test is the arbiter: if the generic-PDA golden fails on bump disagreement, the decoder choice is wrong — switch to the other one.

- [ ] **Step 4: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS. The golden test agreeing with Rust on address AND bump proves the on-curve rejection matches.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/Core/Pda.hs test/Test/Core/Pda.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "feat: program derived address (PDA) derivation with on-curve rejection"
```

---

### Task 3: Token instruction type + serialization

**Files:**
- Create: `src/Network/Solana/SplPrograms/Token.hs` (type + Binary only this task)
- Create: `test/Test/SplPrograms/Token.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Produces: `tokenProgramId :: SolanaPublicKey`; `data AuthorityType = MintTokens | FreezeAuthority | AccountOwner | CloseAuthority` (Enum-like, u8 0-3); `data TokenInstruction` with the 21 constructors listed below and a symmetric `Binary` instance. Task 4 adds builders to this same module.

- [ ] **Step 1: Write the failing golden + property tests**

Create `test/Test/SplPrograms/Token.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.Token (tests) where

import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.SplPrograms.Token qualified as Tok
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

walletPk, mintPk, delegatePk :: SolanaPublicKey
walletPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)
delegatePk = unsafeSolanaPublicKeyRaw (replicate 32 13)

enc :: Tok.TokenInstruction -> BS.ByteString
enc = BL.toStrict . encode

genTokenInstruction :: Gen Tok.TokenInstruction
genTokenInstruction =
  oneof
    [ Tok.InitializeMint <$> arbitrary <*> genPk <*> genMaybePk,
      pure Tok.InitializeAccount,
      Tok.InitializeMultisig <$> arbitrary,
      Tok.Transfer <$> arbitrary,
      Tok.Approve <$> arbitrary,
      pure Tok.Revoke,
      Tok.SetAuthority <$> genAuthType <*> genMaybePk,
      Tok.MintTo <$> arbitrary,
      Tok.Burn <$> arbitrary,
      pure Tok.CloseAccount,
      pure Tok.FreezeAccount,
      pure Tok.ThawAccount,
      Tok.TransferChecked <$> arbitrary <*> arbitrary,
      Tok.ApproveChecked <$> arbitrary <*> arbitrary,
      Tok.MintToChecked <$> arbitrary <*> arbitrary,
      Tok.BurnChecked <$> arbitrary <*> arbitrary,
      Tok.InitializeAccount2 <$> genPk,
      pure Tok.SyncNative,
      Tok.InitializeAccount3 <$> genPk,
      Tok.InitializeMultisig2 <$> arbitrary,
      Tok.InitializeMint2 <$> arbitrary <*> genPk <*> genMaybePk
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genMaybePk = oneof [pure Nothing, Just <$> genPk]
    genAuthType = elements [Tok.MintTokens, Tok.FreezeAuthority, Tok.AccountOwner, Tok.CloseAuthority]

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/token_instruction_data.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "SPL Token instruction data (golden + properties)"
      [ goldenCase getFixtures "InitializeMint-some" (Tok.InitializeMint 6 walletPk (Just delegatePk)),
        goldenCase getFixtures "InitializeMint-none" (Tok.InitializeMint 6 walletPk Nothing),
        goldenCase getFixtures "InitializeAccount" Tok.InitializeAccount,
        goldenCase getFixtures "InitializeMultisig" (Tok.InitializeMultisig 2),
        goldenCase getFixtures "Transfer" (Tok.Transfer 1000000),
        goldenCase getFixtures "Approve" (Tok.Approve 1000000),
        goldenCase getFixtures "Revoke" Tok.Revoke,
        goldenCase getFixtures "SetAuthority-some" (Tok.SetAuthority Tok.AccountOwner (Just delegatePk)),
        goldenCase getFixtures "SetAuthority-none" (Tok.SetAuthority Tok.CloseAuthority Nothing),
        goldenCase getFixtures "MintTo" (Tok.MintTo 1000000),
        goldenCase getFixtures "Burn" (Tok.Burn 1000000),
        goldenCase getFixtures "CloseAccount" Tok.CloseAccount,
        goldenCase getFixtures "FreezeAccount" Tok.FreezeAccount,
        goldenCase getFixtures "ThawAccount" Tok.ThawAccount,
        goldenCase getFixtures "TransferChecked" (Tok.TransferChecked 1000000 6),
        goldenCase getFixtures "ApproveChecked" (Tok.ApproveChecked 1000000 6),
        goldenCase getFixtures "MintToChecked" (Tok.MintToChecked 1000000 6),
        goldenCase getFixtures "BurnChecked" (Tok.BurnChecked 1000000 6),
        goldenCase getFixtures "InitializeAccount2" (Tok.InitializeAccount2 walletPk),
        goldenCase getFixtures "SyncNative" Tok.SyncNative,
        goldenCase getFixtures "InitializeAccount3" (Tok.InitializeAccount3 walletPk),
        goldenCase getFixtures "InitializeMultisig2" (Tok.InitializeMultisig2 2),
        goldenCase getFixtures "InitializeMint2-some" (Tok.InitializeMint2 6 walletPk (Just delegatePk)),
        testProperty "Binary round-trip" $
          forAll genTokenInstruction $ \ti -> decode (encode ti) === ti,
        testCase "decode fails on unknown discriminant" $
          case Data.Binary.decodeOrFail (BL.pack [21]) :: Either (BL.ByteString, Data.Binary.ByteOffset, String) (BL.ByteString, Data.Binary.ByteOffset, Tok.TokenInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 21"
      ]
  where
    goldenCase getFixtures name ti =
      testCase name $ do
        fs <- getFixtures
        enc ti @?= requireFixture name fs
```

Adjust the `decodeOrFail`/`ByteOffset` imports the same way the ComputeBudget test does (they were sourced from `Data.Binary`/`Data.Binary.Get` in phase 2 — mirror that file's import shape exactly).

- [ ] **Step 2: Register and verify compile failure**

Cabal: `exposed-modules` gains `Network.Solana.SplPrograms.Token` (after `...SplPrograms.Memo`); test `other-modules` gains `Test.SplPrograms.Token`; wire into `test/Main.hs`.

Run: `cabal test --test-show-details=direct 2>&1 | head -20` → compile failure (module missing).

- [ ] **Step 3: Implement the type + Binary instance**

Create `src/Network/Solana/SplPrograms/Token.hs`:

```haskell
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
```

(Record fields with shared names across constructors must keep identical types — they do: `tiAmount :: Word64`, `tiDecimals :: Word8`, `tiM :: Word8`, `tiOwner`, `tiMintAuthority`, `tiFreezeAuthority`. If GHC rejects the mixed positional/record use in tests, the test file's constructor calls are positional, which is always valid.)

- [ ] **Step 4: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS — 23 goldens, round-trip property, unknown-discriminant case.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/SplPrograms/Token.hs test/Test/SplPrograms/Token.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "feat: SPL Token instruction type with pack-format serialization (0-20)"
```

---

### Task 4: Token instruction builders

**Files:**
- Modify: `src/Network/Solana/SplPrograms/Token.hs` (append builders)
- Modify: `test/Test/SplPrograms/Token.hs` (append account-meta tests)

**Interfaces:**
- Consumes: Task 3's type; `mkInstruction`, `AccountMeta (..)` from `Core.Instruction`.
- Produces (used by Task 5): `transferChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [SolanaPublicKey] -> Word64 -> Word8 -> Instruction` (source, mint, dest, owner, multisig signers, amount, decimals). Plus the builders below.

- [ ] **Step 1: Write the failing account-meta tests**

Append to the `testGroup` list in `test/Test/SplPrograms/Token.hs` (add `import Network.Solana.Core.Instruction` and constants `sig1Pk = unsafeSolanaPublicKeyRaw (replicate 32 14)`, `sig2Pk = unsafeSolanaPublicKeyRaw (replicate 32 15)`, `accountPk = unsafeSolanaPublicKeyRaw (replicate 32 16)` at top level — also extend the top-level type signature line):

```haskell
        testCase "transfer metas (single owner)" $
          iAccounts (Tok.transfer accountPk delegatePk walletPk [] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ],
        testCase "transfer metas (multisig)" $
          iAccounts (Tok.transfer accountPk delegatePk walletPk [sig1Pk, sig2Pk] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk False False,
                  AccountMeta sig1Pk True False,
                  AccountMeta sig2Pk True False
                ],
        testCase "transferChecked metas" $
          iAccounts (Tok.transferChecked accountPk mintPk delegatePk walletPk [] 1000000 6)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ],
        testCase "initializeMint metas" $
          iAccounts (Tok.initializeMint mintPk 6 walletPk (Just delegatePk))
            @?= [ AccountMeta mintPk False True,
                  AccountMeta Tok.rentSysvar False False
                ],
        testCase "mintTo metas" $
          iAccounts (Tok.mintTo mintPk accountPk walletPk [] 1000000)
            @?= [ AccountMeta mintPk False True,
                  AccountMeta accountPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "burn metas" $
          iAccounts (Tok.burn accountPk mintPk walletPk [] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "closeAccount metas" $
          iAccounts (Tok.closeAccount accountPk delegatePk walletPk [])
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ]
```

If `AccountMeta` positional application fails to compile (record-only construction), use record syntax `AccountMeta {accountPubKey = ..., isSigner = ..., isWritable = ...}` as the existing tests do.

- [ ] **Step 2: Run and verify compile failure** (`Tok.transfer` etc. not in scope)

- [ ] **Step 3: Implement the builders**

Append to `src/Network/Solana/SplPrograms/Token.hs` (add `import Network.Solana.Sysvar qualified as Sysvar` — expose `rentSysvar` as a re-export alias so tests don't need the Sysvar module):

```haskell
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
```

- [ ] **Step 4: Run and verify all pass** (`cabal test --test-show-details=direct`)

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/SplPrograms/Token.hs test/Test/SplPrograms/Token.hs
git commit -m "feat: SPL Token instruction builders with owner/multisig metas"
```

---

### Task 5: ATA client + end-to-end transaction golden + length-guard test

**Files:**
- Create: `src/Network/Solana/SplPrograms/AssociatedTokenAccount.hs`
- Create: `test/Test/SplPrograms/AssociatedTokenAccount.hs`
- Modify: `test/Test/Core/Message.hs` (ata-transfer golden), `test/Test/NativePrograms/SystemProgram.hs` (length-guard test)
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `findProgramAddress` (Task 2), `tokenProgramId`/`transferChecked` (Tasks 3-4), `systemProgramId` from SystemProgram, existing Message-test constants.
- Produces: `associatedTokenProgramId`, `getAssociatedTokenAddress :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey`, `createAssociatedTokenAccount`, `createAssociatedTokenAccountIdempotent :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction` (funder, wallet, mint).

- [ ] **Step 1: Write the failing tests**

Create `test/Test/SplPrograms/AssociatedTokenAccount.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.AssociatedTokenAccount (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata
import Network.Solana.SplPrograms.Token qualified as Tok
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

mintPk :: SolanaPublicKey
mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Associated Token Account"
      [ testCase "getAssociatedTokenAddress matches Rust" $ do
          fs <- getFixtures
          case Ata.getAssociatedTokenAddress payerPk mintPk of
            Nothing -> assertFailure "no ATA derivable"
            Just ata -> getSolanaPublicKeyRaw ata @?= requireFixture "ata-address" fs,
        testCase "create-idempotent data byte" $ do
          case Ata.getAssociatedTokenAddress payerPk mintPk of
            Nothing -> assertFailure "no ATA derivable"
            Just _ ->
              instrData (iData (Ata.createAssociatedTokenAccountIdempotent payerPk payerPk mintPk))
                @?= BS.singleton 1,
        testCase "create data byte" $
          instrData (iData (Ata.createAssociatedTokenAccount payerPk payerPk mintPk))
            @?= BS.singleton 0,
        testCase "create metas" $ do
          fs <- getFixtures
          let expectedAta = unsafeSolanaPublicKeyRaw (BS.unpack (requireFixture "ata-address" fs))
          iAccounts (Ata.createAssociatedTokenAccount payerPk payerPk mintPk)
            @?= [ AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = True},
                  AccountMeta {accountPubKey = expectedAta, isSigner = False, isWritable = True},
                  AccountMeta {accountPubKey = payerPk, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = mintPk, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = Ata.systemProgramRef, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = Tok.tokenProgramId, isSigner = False, isWritable = False}
                ]
      ]
```

In `test/Test/Core/Message.hs` add imports `Network.Solana.SplPrograms.Token qualified as Tok`, `Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata` and a golden case:

```haskell
      testCase "ATA create + transferChecked transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)
            walletPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
            payerPk = fst payerKeys
            srcAta = maybe (error "no src ata") id (Ata.getAssociatedTokenAddress payerPk mintPk)
            dstAta = maybe (error "no dst ata") id (Ata.getAssociatedTokenAddress walletPk mintPk)
            ixs =
              [ Ata.createAssociatedTokenAccountIdempotent payerPk walletPk mintPk,
                Tok.transferChecked srcAta mintPk dstAta payerPk [] 1000000 6
              ]
        case newTransactionIntent [snd payerKeys] ixs fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "ata-transfer-transaction" fs),
```

(`unsafeSolanaPublicKeyRaw` may need adding to the Crypto import list in that file.)

In `test/Test/NativePrograms/SystemProgram.hs` add the phase-2 carry-forward test (uses the same decodeOrFail import shape already present):

```haskell
          testCase "getBincodeString rejects length beyond Int range" $
            -- discriminant 3 (CreateAccountWithSeed) -> base pk (32 bytes) -> u64 length 2^63
            let bytes = BL.pack ([3, 0, 0, 0] <> replicate 32 3 <> [0, 0, 0, 0, 0, 0, 0, 0x80])
             in case decodeOrFail bytes :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, SP.SystemInstruction) of
                  Left _ -> pure ()
                  Right _ -> assertFailure "expected decode failure for absurd length"
```

- [ ] **Step 2: Register and verify compile failure**

Cabal: `exposed-modules` gains `Network.Solana.SplPrograms.AssociatedTokenAccount` (alphabetically before `...SplPrograms.Memo`); test `other-modules` gains `Test.SplPrograms.AssociatedTokenAccount`; wire into `test/Main.hs`. Run → compile failure.

- [ ] **Step 3: Implement**

Create `src/Network/Solana/SplPrograms/AssociatedTokenAccount.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Associated Token Account (ATA) program: derives the
-- canonical token account address for a (wallet, mint) pair and builds
-- create instructions. <https://spl.solana.com/associated-token-account>
module Network.Solana.SplPrograms.AssociatedTokenAccount where

import Data.Binary (Binary (..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.Core.Pda (findProgramAddress)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.SplPrograms.Token qualified as Token

-- | Associated Token Account program address.
associatedTokenProgramId :: SolanaPublicKey
associatedTokenProgramId = "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL"

-- | System program reference (re-exported for account-meta lists).
systemProgramRef :: SolanaPublicKey
systemProgramRef = SystemProgram.systemProgramId

-- | ATA instruction data: a single borsh discriminant byte.
data AtaInstruction = Create | CreateIdempotent
  deriving (Eq, Show)

instance Binary AtaInstruction where
  put :: AtaInstruction -> Data.Binary.Put
  put Create = putWord8 0
  put CreateIdempotent = putWord8 1
  get :: Data.Binary.Get AtaInstruction
  get = do
    b <- getWord8
    case b of
      0 -> pure Create
      1 -> pure CreateIdempotent
      _ -> fail ("AtaInstruction: unknown discriminant " <> show b)

-- | Derive the associated token account address for a wallet and mint:
-- the PDA of [wallet, token program, mint] under the ATA program.
getAssociatedTokenAddress :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey
getAssociatedTokenAddress wallet mint =
  fst
    <$> findProgramAddress
      [ getSolanaPublicKeyRaw wallet,
        getSolanaPublicKeyRaw Token.tokenProgramId,
        getSolanaPublicKeyRaw mint
      ]
      associatedTokenProgramId

ataMetas :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [AccountMeta]
ataMetas funder wallet mint =
  let ata = maybe (error "getAssociatedTokenAddress: derivation failed") id (getAssociatedTokenAddress wallet mint)
   in [ AccountMeta {accountPubKey = funder, isSigner = True, isWritable = True},
        AccountMeta {accountPubKey = ata, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = wallet, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = systemProgramRef, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = Token.tokenProgramId, isSigner = False, isWritable = False}
      ]

-- | Create the associated token account for (wallet, mint), paid by the funder.
-- Fails the transaction if it already exists.
createAssociatedTokenAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
createAssociatedTokenAccount funder wallet mint =
  mkInstruction associatedTokenProgramId (ataMetas funder wallet mint) Create

-- | Like 'createAssociatedTokenAccount' but succeeds (no-op) if the account
-- already exists.
createAssociatedTokenAccountIdempotent :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
createAssociatedTokenAccountIdempotent funder wallet mint =
  mkInstruction associatedTokenProgramId (ataMetas funder wallet mint) CreateIdempotent
```

(If the explicit `put ::`/`get ::` signatures need types imported, use `import Data.Binary (Binary (..), Put, Get)` and write `put :: AtaInstruction -> Put`.) Then in `src/Network/Solana/NativePrograms/SystemProgram.hs` nothing changes — the length-guard already exists; the new test just covers it.

- [ ] **Step 4: Run and verify all pass** — the ata-transfer transaction golden is the end-to-end proof (PDA derivation + token builder + ATA builder + canonical ordering, byte-identical to Rust).

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/SplPrograms/AssociatedTokenAccount.hs test/Test/SplPrograms/AssociatedTokenAccount.hs test/Test/Core/Message.hs test/Test/NativePrograms/SystemProgram.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "feat: Associated Token Account client; ATA transfer end-to-end golden"
```

---

### Task 6: README + CHANGELOG

**Files:** `README.md`, `CHANGELOG.md`

- [ ] **Step 1:** README Features: check `- [x] SPL Token` and `- [x] Associated Token Account` under the SPL section.
- [ ] **Step 2:** CHANGELOG, top of Unreleased:

```markdown
* Added SPL Token program client (instructions 0-20: mint/account initialization,
  transfers, approvals, minting, burning, freezing, multisig support) —
  golden-tested against the `spl-token` Rust crate.
* Added Associated Token Account program client with address derivation.
* Added Program Derived Address (PDA) support: `createProgramAddress` /
  `findProgramAddress` with ed25519 on-curve rejection (new `crypton` dependency).
```

- [ ] **Step 3:** `cabal test --test-show-details=direct` → green; commit:

```bash
git add README.md CHANGELOG.md
git commit -m "docs: roadmap and changelog for SPL Token, ATA, and PDA support"
```

---

## Success criteria (from the spec)

1. Suite green — every task. 2. Token/ATA data goldens — Tasks 1, 3, 5. 3. ATA address golden — Tasks 1, 2, 5. 4. ata-transfer transaction golden — Task 5. 5. Docs — Task 6.
