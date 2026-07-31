# Core Verification & Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove the SDK's serialization is byte-identical to Solana's wire format with golden-vector + property tests, fix the wire-format bugs, complete all 13 System Program instructions, and add CI.

**Architecture:** A tasty test suite consumes JSON golden fixtures generated once by a small Rust program (`tools/fixture-gen`) using the official `solana-sdk`. Tests land before fixes (TDD). Fixes are confined to `Core.Crypto`, `Core.Instruction`, `Core.Message`, `Core.Compact`, and `NativePrograms.SystemProgram`.

**Tech Stack:** Haskell (GHC 9.6.7, cabal), tasty + tasty-hunit + tasty-quickcheck, Rust (cargo 1.85, `solana-sdk` 1.18 + `bincode` 1.3) for fixture generation only.

**Spec:** `docs/superpowers/specs/2026-07-31-core-verification-hardening-design.md`

## Global Constraints

- Work on branch `development`. Commit at the end of every task.
- `cabal build all` must stay warning-clean (`-Wall` is on via the `warnings` common stanza).
- **No new library dependencies.** New deps go in the test-suite stanza only: `tasty`, `tasty-hunit`, `tasty-quickcheck`, `base16-bytestring`, `aeson`, `binary`, `bytestring`, `containers`, `text`.
- Golden fixtures are generated ONCE by `tools/fixture-gen` (Rust; cargo is installed at `~/.cargo/bin/cargo`) and committed to `test/fixtures/`. CI never runs cargo.
- Fixed test constants — the Haskell tests and the Rust generator MUST use exactly these values:
  - payer keypair seed: 32 bytes of `1`
  - recipient pubkey: 32 bytes of `2`
  - base pubkey: 32 bytes of `3`
  - owner pubkey: 32 bytes of `5`
  - authority pubkey: 32 bytes of `6`
  - funder pubkey: 32 bytes of `7`; new-account pubkey: 32 bytes of `8`
  - blockhash: 32 bytes of `9`
  - seed string: `"hello-seed"`; lamports: `1_000_000`; space: `165`; transfer amount: `1_000_000_000`; allocate space: `200`
- Match existing code style: explicit type signatures inside instances, `qualified` imports, record syntax for `AccountMeta`.

## Known bugs this plan fixes (all verified by reading code)

1. **Pubkey length prefix** — `SystemProgram.hs` serializes pubkeys with `put (getSolanaPublicKeyRaw k)`; the `Binary ByteString` instance writes an 8-byte length prefix first. Wire-invalid for `CreateAccount`, `Assign`, `CreateAccountWithSeed`. Fix: `putByteString`.
2. **Seed encoding** — `CreateAccountWithSeed` writes the seed with `putStringUtf8` (no length). bincode requires u64 LE byte-length + UTF-8 bytes.
3. **Signature decode** — `Binary SolanaSignature` `get` reads 32 bytes; Ed25519 signatures are 64 (`Crypto.hs:82`).
4. **Program-id account** — `mkInstruction` appends the program id as an `AccountMeta`, so every compiled instruction passes the program itself as a spurious account. The program id belongs only in the message account-keys list (readonly, unsigned).
5. **Aliased compact-u16** — the decoder accepts non-canonical encodings like `[0x80, 0x00]`; the Rust decoder rejects them.
6. **`fromJust` parse crash** — `FromJSON CompiledInstruction` crashes on invalid base58 instead of failing the parse.

---

### Task 1: Test suite scaffolding

**Files:**
- Modify: `solana-haskell-sdk.cabal` (test-suite stanza)
- Modify: `test/Main.hs`

**Interfaces:**
- Produces: a running (empty) tasty suite; later tasks add `Test.*` modules to `other-modules` and one entry each to the `testGroup` list in `test/Main.hs`.

- [ ] **Step 1: Replace the test-suite stanza in `solana-haskell-sdk.cabal`**

Replace the whole `test-suite solana-haskell-sdk-test` block with:

```cabal
test-suite solana-haskell-sdk-test
  import:           warnings
  default-language: GHC2021
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Main.hs
  build-depends:
    , aeson
    , base                ^>=4.18.0.0
    , base16-bytestring
    , binary
    , bytestring
    , containers
    , solana-haskell-sdk
    , tasty
    , tasty-hunit
    , tasty-quickcheck
    , text
```

- [ ] **Step 2: Replace `test/Main.hs`**

```haskell
module Main (main) where

import Test.Tasty

main :: IO ()
main = defaultMain (testGroup "solana-haskell-sdk" [])
```

- [ ] **Step 3: Run the suite**

Run: `cabal test --test-show-details=direct`
Expected: PASS (0 tests). Dependency resolution may download tasty packages on first run.

- [ ] **Step 4: Commit**

```bash
git add solana-haskell-sdk.cabal test/Main.hs
git commit -m "test: add tasty test-suite scaffolding"
```

---

### Task 2: compact-u16 tests + alias rejection fix

**Files:**
- Create: `test/Test/Core/Compact.hs`
- Modify: `src/Network/Solana/Core/Compact.hs` (decoder only)
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `encodeCompactU16 :: Word16 -> BL.ByteString`, `decodeCompactU16 :: BL.ByteString -> Either String Word16` from `Network.Solana.Core.Compact`.
- Produces: `Test.Core.Compact.tests :: TestTree`.

- [ ] **Step 1: Write the tests**

Create `test/Test/Core/Compact.hs`:

```haskell
module Test.Core.Compact (tests) where

import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Compact
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "compact-u16"
    [ testCase "known encodings" $ do
        BL.unpack (encodeCompactU16 0) @?= [0x00]
        BL.unpack (encodeCompactU16 1) @?= [0x01]
        BL.unpack (encodeCompactU16 127) @?= [0x7f]
        BL.unpack (encodeCompactU16 128) @?= [0x80, 0x01]
        BL.unpack (encodeCompactU16 16383) @?= [0xff, 0x7f]
        BL.unpack (encodeCompactU16 16384) @?= [0x80, 0x80, 0x01]
        BL.unpack (encodeCompactU16 65535) @?= [0xff, 0xff, 0x03],
      testProperty "decode . encode == id" $ \w ->
        decodeCompactU16 (encodeCompactU16 w) === Right w,
      testProperty "encoded length matches value range" $ \w ->
        let len = fromIntegral (BL.length (encodeCompactU16 w)) :: Int
         in if w < 0x80
              then len === 1
              else
                if w < 0x4000
                  then len === 2
                  else len === 3,
      testCase "rejects overlong encoding" $
        assertLeft (decodeCompactU16 (BL.pack [0x80, 0x80, 0x80, 0x01])),
      testCase "rejects aliased encoding" $
        assertLeft (decodeCompactU16 (BL.pack [0x80, 0x00]))
    ]

assertLeft :: (Show b) => Either a b -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right v) = assertFailure ("expected decode failure, got " <> show v)
```

- [ ] **Step 2: Register the module**

In `solana-haskell-sdk.cabal`, add to the test-suite stanza (before `build-depends`):

```cabal
  other-modules:
    Test.Core.Compact
```

In `test/Main.hs`, import and register:

```haskell
module Main (main) where

import Test.Core.Compact qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain
    ( testGroup
        "solana-haskell-sdk"
        [ Test.Core.Compact.tests
        ]
    )
```

- [ ] **Step 3: Run and verify the alias test fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL — exactly one failure, "rejects aliased encoding" (the current decoder accepts `[0x80, 0x00]` as 0). Everything else passes.

- [ ] **Step 4: Fix the decoder**

In `src/Network/Solana/Core/Compact.hs`, in `getCompactU16`'s `go`, reject a continuation followed by a zero byte. Replace the non-continue branch:

```haskell
          if continue
            then go acc' (byteIndex + 1)
            else
              if byteIndex > 0 && byte == 0
                then fail "Aliased (non-canonical) compact-u16 encoding"
                else
                  if byteIndex == 2 && (byte .&. 0xFC) /= 0
                    then fail "Invalid 3rd byte in compact-u16 (only 2 bits allowed)"
                    else case fromIntegral acc' of
                      w | w <= 0xFFFF -> return w
                      _ -> fail "Decoded value exceeds u16 range"
```

- [ ] **Step 5: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add test/Test/Core/Compact.hs src/Network/Solana/Core/Compact.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "test: compact-u16 golden + property tests; reject aliased encodings"
```

---

### Task 3: Crypto round-trip tests + 64-byte signature fix

**Files:**
- Create: `test/Test/Core/Crypto.hs`
- Modify: `src/Network/Solana/Core/Crypto.hs:82`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `toBase58String`, `fromBase58String`, `toBase64String`, `fromBase64String`, `unsafeSolanaPublicKeyRaw :: [Word8] -> SolanaPublicKey`, `createSolanaKeypairFromSeed :: ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)`, `dsign` from `Network.Solana.Core.Crypto`.
- Produces: `Test.Core.Crypto.tests :: TestTree`.

- [ ] **Step 1: Write the tests**

Create `test/Test/Core/Crypto.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Crypto (tests) where

import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

newtype Bytes32 = Bytes32 BS.ByteString
  deriving (Show)

instance Arbitrary Bytes32 where
  arbitrary = Bytes32 . BS.pack <$> vectorOf 32 arbitrary

tests :: TestTree
tests =
  testGroup
    "crypto"
    [ testProperty "base58 round-trip" $ \(Bytes32 bs) ->
        fromBase58String (toBase58String bs) === Just bs,
      testProperty "base64 round-trip" $ \(Bytes32 bs) ->
        fromBase64String (toBase64String bs) === bs,
      testProperty "public key Binary round-trip" $ \(Bytes32 bs) ->
        let pk = unsafeSolanaPublicKeyRaw (BS.unpack bs)
         in decode (encode pk) === pk,
      testCase "signature Binary round-trip (64 bytes)" $ do
        Just (_, priv) <- pure (createSolanaKeypairFromSeed (BS.replicate 32 1))
        let sig = dsign priv "solana-haskell-sdk"
        decode (encode sig) @?= sig
    ]
```

- [ ] **Step 2: Register the module**

Add `Test.Core.Crypto` to `other-modules` in the cabal test-suite stanza, and in `test/Main.hs` add `import Test.Core.Crypto qualified` and `Test.Core.Crypto.tests` to the group list.

- [ ] **Step 3: Run and verify the signature test fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL — "signature Binary round-trip (64 bytes)" fails (decoder truncates to 32 bytes). The three properties pass.

- [ ] **Step 4: Fix the decoder**

In `src/Network/Solana/Core/Crypto.hs` line 82, change:

```haskell
  get = SolanaSignature . Ed25519.Signature <$> getByteString 32
```

to:

```haskell
  get = SolanaSignature . Ed25519.Signature <$> getByteString 64
```

- [ ] **Step 5: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add test/Test/Core/Crypto.hs src/Network/Solana/Core/Crypto.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "test: crypto round-trips; fix 64-byte signature decoding"
```

---

### Task 4: Golden-vector generator (Rust) + fixture loader

**Files:**
- Create: `tools/fixture-gen/Cargo.toml`, `tools/fixture-gen/src/main.rs`, `tools/fixture-gen/.gitignore`, `tools/README.md`
- Create: `test/fixtures/system_instruction_data.json`, `test/fixtures/messages.json`, `test/fixtures/transactions.json` (generated, committed)
- Create: `test/Test/Fixtures.hs`
- Modify: `solana-haskell-sdk.cabal`

**Interfaces:**
- Produces: `Test.Fixtures.loadFixtures :: FilePath -> IO [Fixture]`, `Test.Fixtures.requireFixture :: String -> [Fixture] -> BS.ByteString`. Fixture JSON schema: array of `{"name": string, "hex": string}`.

- [ ] **Step 1: Write the generator**

Create `tools/fixture-gen/.gitignore`:

```
target/
```

Create `tools/fixture-gen/Cargo.toml`:

```toml
[package]
name = "fixture-gen"
version = "0.1.0"
edition = "2021"

[dependencies]
solana-sdk = "1.18"
bincode = "1.3"
serde_json = "1"
```

Create `tools/fixture-gen/src/main.rs`:

```rust
use serde_json::json;
use solana_sdk::{
    hash::Hash,
    message::Message,
    pubkey::Pubkey,
    signature::keypair_from_seed,
    signer::Signer,
    system_instruction::{self, SystemInstruction},
    transaction::Transaction,
};
use std::fs;

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn main() {
    fs::create_dir_all("../../test/fixtures").unwrap();

    let base = Pubkey::new_from_array([3u8; 32]);
    let owner = Pubkey::new_from_array([5u8; 32]);
    let authority = Pubkey::new_from_array([6u8; 32]);
    let seed = "hello-seed".to_string();
    let lamports: u64 = 1_000_000;
    let space: u64 = 165;

    let variants: Vec<(&str, SystemInstruction)> = vec![
        ("CreateAccount", SystemInstruction::CreateAccount { lamports, space, owner }),
        ("Assign", SystemInstruction::Assign { owner }),
        ("Transfer", SystemInstruction::Transfer { lamports }),
        (
            "CreateAccountWithSeed",
            SystemInstruction::CreateAccountWithSeed {
                base,
                seed: seed.clone(),
                lamports,
                space,
                owner,
            },
        ),
        ("AdvanceNonceAccount", SystemInstruction::AdvanceNonceAccount),
        ("WithdrawNonceAccount", SystemInstruction::WithdrawNonceAccount(lamports)),
        ("InitializeNonceAccount", SystemInstruction::InitializeNonceAccount(authority)),
        ("AuthorizeNonceAccount", SystemInstruction::AuthorizeNonceAccount(authority)),
        ("Allocate", SystemInstruction::Allocate { space }),
        (
            "AllocateWithSeed",
            SystemInstruction::AllocateWithSeed { base, seed: seed.clone(), space, owner },
        ),
        (
            "AssignWithSeed",
            SystemInstruction::AssignWithSeed { base, seed: seed.clone(), owner },
        ),
        (
            "TransferWithSeed",
            SystemInstruction::TransferWithSeed {
                lamports,
                from_seed: seed.clone(),
                from_owner: owner,
            },
        ),
        ("UpgradeNonceAccount", SystemInstruction::UpgradeNonceAccount),
    ];

    let instr_fixtures: Vec<_> = variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/system_instruction_data.json",
        serde_json::to_string_pretty(&instr_fixtures).unwrap(),
    )
    .unwrap();

    let payer = keypair_from_seed(&[1u8; 32]).unwrap();
    let recipient = Pubkey::new_from_array([2u8; 32]);
    let blockhash = Hash::new_from_array([9u8; 32]);

    let transfer_ix = system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000);
    let msg_transfer =
        Message::new_with_blockhash(&[transfer_ix.clone()], Some(&payer.pubkey()), &blockhash);

    let allocate_ix = system_instruction::allocate(&payer.pubkey(), 200);
    let msg_two = Message::new_with_blockhash(
        &[transfer_ix, allocate_ix],
        Some(&payer.pubkey()),
        &blockhash,
    );

    let msg_fixtures = vec![
        json!({ "name": "transfer-message", "hex": hex(&msg_transfer.serialize()) }),
        json!({ "name": "transfer-allocate-message", "hex": hex(&msg_two.serialize()) }),
    ];
    fs::write(
        "../../test/fixtures/messages.json",
        serde_json::to_string_pretty(&msg_fixtures).unwrap(),
    )
    .unwrap();

    let tx = Transaction::new(&[&payer], msg_transfer, blockhash);
    let tx_fixtures = vec![
        json!({ "name": "transfer-transaction", "hex": hex(&bincode::serialize(&tx).unwrap()) }),
        json!({ "name": "payer-pubkey", "hex": hex(&payer.pubkey().to_bytes()) }),
    ];
    fs::write(
        "../../test/fixtures/transactions.json",
        serde_json::to_string_pretty(&tx_fixtures).unwrap(),
    )
    .unwrap();

    println!("fixtures written to test/fixtures/");
}
```

Create `tools/README.md`:

```markdown
# tools/fixture-gen

Generates the golden test vectors in `test/fixtures/` from the official Rust
`solana-sdk`, so the Haskell SDK's serialization can be asserted byte-identical
to the reference implementation.

Fixtures are committed; regeneration is only needed when adding vectors:

    cd tools/fixture-gen
    cargo run

All fixed inputs (seeds, pubkey byte patterns, amounts) are mirrored as
constants in the Haskell test modules — if you change them here, change them
there too.
```

- [ ] **Step 2: Run the generator**

Run: `cd tools/fixture-gen && cargo run && cd ../..`
Expected: prints `fixtures written to test/fixtures/`; first run compiles solana-sdk (a few minutes).

If `solana-sdk = "1.18"` fails to compile (older solana crates have known build breakage with newer rustc versions, e.g. via `ahash`), change the dependency to `solana-sdk = "2"` — every API used in `main.rs` is unchanged in 2.x.

- [ ] **Step 3: Sanity-check the fixtures**

Run: `python3 -m json.tool test/fixtures/system_instruction_data.json > /dev/null && python3 -m json.tool test/fixtures/messages.json > /dev/null && python3 -m json.tool test/fixtures/transactions.json > /dev/null && grep -o '"name"' test/fixtures/system_instruction_data.json | wc -l`
Expected: valid JSON; 13 names in `system_instruction_data.json`. Spot-check: the `Transfer` entry's hex must be exactly `0200000040420f0000000000` (u32 LE discriminant 2 + u64 LE 1000000).

- [ ] **Step 4: Write the fixture loader**

Create `test/Test/Fixtures.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Fixtures
  ( Fixture (..),
    loadFixtures,
    requireFixture,
  )
where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data Fixture = Fixture
  { fixtureName :: String,
    fixtureHex :: T.Text
  }
  deriving (Show)

instance FromJSON Fixture where
  parseJSON = withObject "Fixture" $ \v ->
    Fixture <$> v .: "name" <*> v .: "hex"

loadFixtures :: FilePath -> IO [Fixture]
loadFixtures path = either fail pure =<< eitherDecodeFileStrict path

requireFixture :: String -> [Fixture] -> BS.ByteString
requireFixture name fs =
  case filter ((== name) . fixtureName) fs of
    [f] -> either (error . badHex) id (B16.decode (TE.encodeUtf8 (fixtureHex f)))
    _ -> error ("fixture not found (or duplicated): " <> name)
  where
    badHex err = "bad hex in fixture " <> name <> ": " <> err
```

- [ ] **Step 5: Register and build**

Add `Test.Fixtures` to `other-modules` in the cabal test-suite stanza.

Run: `cabal build solana-haskell-sdk-test`
Expected: compiles clean.

- [ ] **Step 6: Commit**

```bash
git add tools/ test/fixtures/ test/Test/Fixtures.hs solana-haskell-sdk.cabal
git commit -m "test: Rust golden-vector generator, committed fixtures, fixture loader"
```

---

### Task 5: Instruction-data golden tests + pubkey/seed serialization fixes

**Files:**
- Create: `test/Test/NativePrograms/SystemProgram.hs`
- Modify: `src/Network/Solana/NativePrograms/SystemProgram.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `Test.Fixtures` (Task 4); builders `createAccount`, `assignAccount`, `transfer`, `createAccountWithSeed` and accessors `iData`, `instrData`, `iAccounts` from the library.
- Produces: `Test.NativePrograms.SystemProgram.tests :: TestTree`; shared test constants `basePk`, `ownerPk`, `authorityPk`, `funderPk`, `newAccountPk`, `seedStr`, `dataOf` used by later steps in this same module; `putBincodeString :: String -> Put` in `SystemProgram.hs` (reused by Tasks 8-9).

- [ ] **Step 1: Write the golden tests for the 4 existing variants**

Create `test/Test/NativePrograms/SystemProgram.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.SystemProgram (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

basePk, ownerPk, authorityPk, funderPk, newAccountPk :: SolanaPublicKey
basePk = unsafeSolanaPublicKeyRaw (replicate 32 3)
ownerPk = unsafeSolanaPublicKeyRaw (replicate 32 5)
authorityPk = unsafeSolanaPublicKeyRaw (replicate 32 6)
funderPk = unsafeSolanaPublicKeyRaw (replicate 32 7)
newAccountPk = unsafeSolanaPublicKeyRaw (replicate 32 8)

seedStr :: String
seedStr = "hello-seed"

dataOf :: Instruction -> BS.ByteString
dataOf = instrData . iData

tests :: TestTree
tests =
  withResource
    (loadFixtures "test/fixtures/system_instruction_data.json")
    (const (pure ()))
    $ \getFixtures ->
      testGroup
        "SystemProgram instruction data (golden)"
        [ testCase "CreateAccount" $ do
            fs <- getFixtures
            dataOf (SP.createAccount funderPk newAccountPk 1000000 165 ownerPk)
              @?= requireFixture "CreateAccount" fs,
          testCase "Assign" $ do
            fs <- getFixtures
            dataOf (SP.assignAccount newAccountPk ownerPk)
              @?= requireFixture "Assign" fs,
          testCase "Transfer" $ do
            fs <- getFixtures
            dataOf (SP.transfer funderPk newAccountPk 1000000)
              @?= requireFixture "Transfer" fs,
          testCase "CreateAccountWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.createAccountWithSeed basePk seedStr funderPk newAccountPk 1000000 165 ownerPk)
              @?= requireFixture "CreateAccountWithSeed" fs
        ]
```

(The `withResource` wrapper loads the fixture file once for the whole group.)

- [ ] **Step 2: Register the module**

Add `Test.NativePrograms.SystemProgram` to `other-modules`; in `test/Main.hs` add `import Test.NativePrograms.SystemProgram qualified` and `Test.NativePrograms.SystemProgram.tests` to the group list.

- [ ] **Step 3: Run and verify the expected failures**

Run: `cabal test --test-show-details=direct`
Expected: FAIL — `CreateAccount`, `Assign`, `CreateAccountWithSeed` fail (8-byte length prefixes before pubkeys; seed missing its u64 length). `Transfer` passes.

- [ ] **Step 4: Fix the serialization**

In `src/Network/Solana/NativePrograms/SystemProgram.hs`:

Add imports after the existing ones:

```haskell
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
```

Add the helper below the `Binary` instance:

```haskell
-- bincode encodes a Rust String as a u64 little-endian byte length
-- followed by the UTF-8 bytes
putBincodeString :: String -> Put
putBincodeString s = do
  let bs = TE.encodeUtf8 (T.pack s)
  putWord64le (fromIntegral (BS.length bs))
  putByteString bs
```

Replace the `put` implementation of the `Binary SystemInstruction` instance:

```haskell
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
```

- [ ] **Step 5: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add test/Test/NativePrograms/SystemProgram.hs src/Network/Solana/NativePrograms/SystemProgram.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "fix: System Program instruction data wire format (pubkey length prefix, bincode seed)"
```

---

### Task 6: Program-id account fix + message/transaction golden tests

**Files:**
- Create: `test/Test/Core/Instruction.hs`, `test/Test/Core/Message.hs`
- Modify: `src/Network/Solana/Core/Instruction.hs` (`mkInstruction`)
- Modify: `src/Network/Solana/Core/Message.hs` (`updateMessageWithInstruction`)
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Consumes: `Test.Fixtures`; `newMessage :: BlockHash -> [Instruction] -> Either CompileException ByteString`, `newTransactionIntent`, `BlockHash (..)` (constructor is exported — `Core.Block` has no export list), `toBase64String`.
- Produces: `Test.Core.Instruction.tests`, `Test.Core.Message.tests`. After this task `mkInstruction` no longer appends the program id to `iAccounts`; message compilation adds it to the account keys as readonly-unsigned.

- [ ] **Step 1: Write the unit test for mkInstruction**

Create `test/Test/Core/Instruction.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Instruction (tests) where

import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Test.Tasty
import Test.Tasty.HUnit

progPk, accPk :: SolanaPublicKey
progPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
accPk = unsafeSolanaPublicKeyRaw (replicate 32 12)

tests :: TestTree
tests =
  testGroup
    "instruction"
    [ testCase "mkInstruction keeps exactly the given account metas" $ do
        let meta = AccountMeta {accountPubKey = accPk, isSigner = True, isWritable = True}
            ix = mkInstruction progPk [meta] ()
        iAccounts ix @?= [meta]
        iProgramId ix @?= progPk
    ]
```

- [ ] **Step 2: Write the message/transaction golden tests**

Create `test/Test/Core/Message.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Message (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Message
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

payerKeys :: (SolanaPublicKey, SolanaPrivateKey)
payerKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just kp -> kp
    Nothing -> error "failed to derive payer keypair from seed"

tests :: TestTree
tests =
  testGroup
    "message and transaction (golden)"
    [ testCase "payer pubkey from seed matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        getSolanaPublicKeyRaw (fst payerKeys) @?= requireFixture "payer-pubkey" fs,
      testCase "single transfer message bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/messages.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newMessage fixedBlockhash [ix] of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= requireFixture "transfer-message" fs,
      testCase "signed transfer transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntent [snd payerKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "transfer-transaction" fs)
    ]
```

- [ ] **Step 3: Register both modules**

Add `Test.Core.Instruction` and `Test.Core.Message` to `other-modules`; in `test/Main.hs` add both imports and both `tests` entries.

- [ ] **Step 4: Run and verify the expected failures**

Run: `cabal test --test-show-details=direct`
Expected: FAIL — "mkInstruction keeps exactly the given account metas" fails (extra appended program-id meta); the two message/transaction golden tests fail (spurious account index in compiled instructions). "payer pubkey from seed matches Rust" must PASS — if it fails, STOP: the Haskell and Rust seed-derivation conventions differ, and the fixtures for messages/transactions are unusable as-is. Investigate before continuing.

- [ ] **Step 5: Fix mkInstruction**

In `src/Network/Solana/Core/Instruction.hs`, replace `mkInstruction`:

```haskell
mkInstruction :: (Binary.Binary a) => SolanaPublicKey -> [AccountMeta] -> a -> Instruction
mkInstruction programid accmetas instrData =
  Instruction
    { iProgramId = programid,
      iAccounts = accmetas,
      iData = InstructionData $ S.toStrict (Binary.encode instrData)
    }
```

- [ ] **Step 6: Fix message compilation**

In `src/Network/Solana/Core/Message.hs`, replace `updateMessageWithInstruction` so the program id still reaches the message account keys (readonly, unsigned):

```haskell
updateMessageWithInstruction :: Message -> Instruction -> Message
updateMessageWithInstruction (Message header accountKeys bh instrs) newInstr =
  let programMeta =
        AccountMeta
          { accountPubKey = iProgramId newInstr,
            isSigner = False,
            isWritable = False
          }
      (newHeader, newAccountKeys) =
        foldl' updateHeaderAndKeys (header, accountKeys) (iAccounts newInstr <> [programMeta])
      newInstrucionsList = instrs <> [newInstr]
   in Message newHeader newAccountKeys bh newInstrucionsList
```

- [ ] **Step 7: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS — including byte-identical message and signed transaction.

- [ ] **Step 8: Commit**

```bash
git add test/Test/Core/Instruction.hs test/Test/Core/Message.hs src/Network/Solana/Core/Instruction.hs src/Network/Solana/Core/Message.hs solana-haskell-sdk.cabal test/Main.hs
git commit -m "fix: program id no longer passed as spurious instruction account; message/tx golden tests"
```

---

### Task 7: Message invariant property tests

**Files:**
- Modify: `src/Network/Solana/Core/Message.hs` (export list only)
- Modify: `test/Test/Core/Message.hs`

**Interfaces:**
- Consumes: internals newly exported from `Network.Solana.Core.Message`: `Message (..)`, `MessageHeader (..)`, `mkNewMessage :: BlockHash -> [Instruction] -> Message`.
- Produces: property `message header/key-ordering invariants` inside `Test.Core.Message.tests`.

- [ ] **Step 1: Extend the Message export list**

In `src/Network/Solana/Core/Message.hs`, change the module header to:

```haskell
module Network.Solana.Core.Message
  ( newTransactionIntent,
    Message (..),
    MessageHeader (..),
    CompiledMessage,
    newMessage,
    newMessageToBase64String,
    mkNewMessage,
  )
where
```

- [ ] **Step 2: Add the property test**

In `test/Test/Core/Message.hs`, add imports:

```haskell
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Network.Solana.Core.Instruction
import Test.Tasty.QuickCheck
```

Add below the existing definitions:

```haskell
keyPool :: [SolanaPublicKey]
keyPool = [unsafeSolanaPublicKeyRaw (replicate 32 b) | b <- [10 .. 15]]

genInstruction :: Gen Instruction
genInstruction = do
  progId <- elements keyPool
  n <- chooseInt (1, 4)
  metas <- vectorOf n genMeta
  pure (mkInstruction progId metas ())
  where
    genMeta = do
      k <- elements keyPool
      s <- arbitrary
      w <- arbitrary
      pure AccountMeta {accountPubKey = k, isSigner = s, isWritable = w}

expectedPrivileges :: [Instruction] -> Map.Map SolanaPublicKey (Bool, Bool)
expectedPrivileges instrs = Map.fromListWith merge (concatMap entries instrs)
  where
    merge (s1, w1) (s2, w2) = (s1 || s2, w1 || w2)
    entries i =
      (iProgramId i, (False, False))
        : [(accountPubKey m, (isSigner m, isWritable m)) | m <- iAccounts i]

prop_messageInvariants :: Property
prop_messageInvariants =
  forAll (resize 5 (listOf1 genInstruction)) $ \instrs ->
    let msg = mkNewMessage fixedBlockhash instrs
        keys = mAccountKeys msg
        privs = expectedPrivileges instrs
        hdr = mHeader msg
        nrs = fromIntegral (numRequiredSignatures hdr)
        nros = fromIntegral (numReadonlySignedAccounts hdr)
        nrou = fromIntegral (numReadonlyUnsignedAccounts hdr)
        (signed, unsigned) = splitAt nrs keys
        (sw, sr) = splitAt (nrs - nros) signed
        (uw, ur) = splitAt (length unsigned - nrou) unsigned
        signerOf k = maybe False fst (Map.lookup k privs)
        writableOf k = maybe False snd (Map.lookup k privs)
     in conjoin
          [ counterexample "duplicate keys" (keys === nub keys),
            counterexample "key set mismatch" (Map.keysSet privs === Set.fromList keys),
            counterexample "non-signer in signed section" (property (all signerOf signed)),
            counterexample "signer in unsigned section" (property (all (not . signerOf) unsigned)),
            counterexample "readonly key in writable section" (property (all writableOf (sw <> uw))),
            counterexample "writable key in readonly section" (property (all (not . writableOf) (sr <> ur)))
          ]
```

Register it inside the `testGroup` list in `tests`:

```haskell
      testProperty "message header/key-ordering invariants" prop_messageInvariants
```

- [ ] **Step 3: Run**

Run: `cabal test --test-show-details=direct`
Expected: PASS. If the property finds a counterexample, that is a real bug in `updateHeaderAndKeys` — debug with the shrunk counterexample before proceeding; do not weaken the property.

- [ ] **Step 4: Commit**

```bash
git add src/Network/Solana/Core/Message.hs test/Test/Core/Message.hs
git commit -m "test: property tests for message header and key-ordering invariants"
```

---

### Task 8: System Program — nonce instructions (indices 4-7, 12)

**Files:**
- Modify: `src/Network/Solana/NativePrograms/SystemProgram.hs`
- Modify: `test/Test/NativePrograms/SystemProgram.hs`

**Interfaces:**
- Consumes: `putBincodeString` (Task 5), sysvars `recentBlockhashes`, `rent` from `Network.Solana.Sysvar`, test constants from Task 5.
- Produces: constructors `AdvanceNonceAccount`, `WithdrawNonceAccount {lamports}`, `InitializeNonceAccount {authority}`, `AuthorizeNonceAccount {authority}`, `UpgradeNonceAccount`; builders `advanceNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction`, `withdrawNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction`, `initializeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> Instruction`, `authorizeNonceAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction`, `upgradeNonceAccount :: SolanaPublicKey -> Instruction`.

- [ ] **Step 1: Write the failing tests**

In `test/Test/NativePrograms/SystemProgram.hs`, extend the `testGroup` list with (after the `CreateAccountWithSeed` case):

```haskell
          testCase "AdvanceNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.advanceNonceAccount newAccountPk authorityPk)
              @?= requireFixture "AdvanceNonceAccount" fs,
          testCase "WithdrawNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.withdrawNonceAccount newAccountPk authorityPk funderPk 1000000)
              @?= requireFixture "WithdrawNonceAccount" fs,
          testCase "InitializeNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.initializeNonceAccount newAccountPk authorityPk)
              @?= requireFixture "InitializeNonceAccount" fs,
          testCase "AuthorizeNonceAccount" $ do
            fs <- getFixtures
            -- args: nonce account, current authority, NEW authority.
            -- The fixture serializes the new authority, which must be authorityPk ([6;32]).
            dataOf (SP.authorizeNonceAccount newAccountPk basePk authorityPk)
              @?= requireFixture "AuthorizeNonceAccount" fs,
          testCase "UpgradeNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.upgradeNonceAccount newAccountPk)
              @?= requireFixture "UpgradeNonceAccount" fs,
          testCase "advanceNonceAccount account metas" $
            iAccounts (SP.advanceNonceAccount newAccountPk authorityPk)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = authorityPk, isSigner = True, isWritable = False}
                  ],
          testCase "withdrawNonceAccount account metas" $
            iAccounts (SP.withdrawNonceAccount newAccountPk authorityPk funderPk 1000000)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = funderPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = authorityPk, isSigner = True, isWritable = False}
                  ]
```

Add the import at the top of the test module:

```haskell
import Network.Solana.Sysvar qualified as Sysvar
```

- [ ] **Step 2: Run and verify compile failure**

Run: `cabal test --test-show-details=direct 2>&1 | head -30`
Expected: FAIL to compile — `advanceNonceAccount` etc. not in scope.

- [ ] **Step 3: Implement**

In `src/Network/Solana/NativePrograms/SystemProgram.hs`:

Add import:

```haskell
import Network.Solana.Sysvar qualified as Sysvar
```

Extend the `SystemInstruction` data type with new constructors (after `CreateAccountWithSeed`):

```haskell
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
```

Extend the `Binary` instance `put`:

```haskell
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
```

Add the builders at the end of the file:

```haskell
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
```

- [ ] **Step 4: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/NativePrograms/SystemProgram.hs test/Test/NativePrograms/SystemProgram.hs
git commit -m "feat: nonce account System Program instructions (4-7, 12)"
```

---

### Task 9: System Program — seed instructions (indices 8-11) + multi-instruction message golden

**Files:**
- Modify: `src/Network/Solana/NativePrograms/SystemProgram.hs`
- Modify: `test/Test/NativePrograms/SystemProgram.hs`
- Modify: `test/Test/Core/Message.hs`

**Interfaces:**
- Consumes: `putBincodeString`, test constants (Task 5), `fixedBlockhash`/`payerKeys`/`recipientPk` (Task 6).
- Produces: constructors `Allocate {space}`, `AllocateWithSeed {base, seed, space, owner}`, `AssignWithSeed {base, seed, owner}`, `TransferWithSeed {lamports, fromSeed, fromOwner}`; builders `allocate :: SolanaPublicKey -> Int -> Instruction`, `allocateWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> Int -> SolanaPublicKey -> Instruction`, `assignWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> SolanaPublicKey -> Instruction`, `transferWithSeed :: SolanaPublicKey -> SolanaPublicKey -> String -> SolanaPublicKey -> SolanaPublicKey -> Lamport -> Instruction`.

- [ ] **Step 1: Write the failing tests**

In `test/Test/NativePrograms/SystemProgram.hs`, extend the `testGroup` list:

```haskell
          testCase "Allocate" $ do
            fs <- getFixtures
            dataOf (SP.allocate newAccountPk 165)
              @?= requireFixture "Allocate" fs,
          testCase "AllocateWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.allocateWithSeed newAccountPk basePk seedStr 165 ownerPk)
              @?= requireFixture "AllocateWithSeed" fs,
          testCase "AssignWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.assignWithSeed newAccountPk basePk seedStr ownerPk)
              @?= requireFixture "AssignWithSeed" fs,
          testCase "TransferWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.transferWithSeed funderPk basePk seedStr ownerPk newAccountPk 1000000)
              @?= requireFixture "TransferWithSeed" fs,
          testCase "allocateWithSeed account metas" $
            iAccounts (SP.allocateWithSeed newAccountPk basePk seedStr 165 ownerPk)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = basePk, isSigner = True, isWritable = False}
                  ],
          testCase "transferWithSeed account metas" $
            iAccounts (SP.transferWithSeed funderPk basePk seedStr ownerPk newAccountPk 1000000)
              @?= [ AccountMeta {accountPubKey = funderPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = basePk, isSigner = True, isWritable = False},
                    AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True}
                  ]
```

In `test/Test/Core/Message.hs`, add to the `testGroup` list:

```haskell
      testCase "transfer + allocate message bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/messages.json"
        let transferIx = SP.transfer (fst payerKeys) recipientPk 1000000000
            allocateIx = SP.allocate (fst payerKeys) 200
        case newMessage fixedBlockhash [transferIx, allocateIx] of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= requireFixture "transfer-allocate-message" fs,
```

- [ ] **Step 2: Run and verify compile failure**

Run: `cabal test --test-show-details=direct 2>&1 | head -30`
Expected: FAIL to compile — `allocate` etc. not in scope.

- [ ] **Step 3: Implement**

In `src/Network/Solana/NativePrograms/SystemProgram.hs`, extend `SystemInstruction` (after `AuthorizeNonceAccount`, before `UpgradeNonceAccount` to keep declaration order matching indices 8-11 then 12):

```haskell
  | -- | Allocate space in a (possibly new) account without funding
    Allocate
      { -- | Number of bytes of memory to allocate
        space :: Word64
      }
  | -- | Allocate space for and assign an account at an address derived from a base public key and a seed
    AllocateWithSeed
      { -- | Base public key
        base :: SolanaPublicKey,
        -- | String of ASCII chars, no longer than `maxSeedLen`
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
        -- | String of ASCII chars, no longer than `maxSeedLen`
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
```

Extend the `Binary` instance `put`:

```haskell
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
```

Add the builders:

```haskell
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
```

- [ ] **Step 4: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS — including the two-instruction message golden test.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/NativePrograms/SystemProgram.hs test/Test/NativePrograms/SystemProgram.hs test/Test/Core/Message.hs
git commit -m "feat: allocate/assign/transfer-with-seed System Program instructions (8-11)"
```

---

### Task 10: CompiledInstruction FromJSON robustness

**Files:**
- Modify: `src/Network/Solana/Core/Instruction.hs`
- Modify: `test/Test/Core/Instruction.hs`

**Interfaces:**
- Consumes: `CompiledInstruction` type and its `FromJSON` instance.
- Produces: parse failure (not a crash) on invalid base58 `data`.

- [ ] **Step 1: Write the failing test**

In `test/Test/Core/Instruction.hs`, add imports:

```haskell
import Data.Aeson (eitherDecode)
```

Add to the `testGroup` list:

```haskell
      testCase "CompiledInstruction FromJSON rejects invalid base58 data" $
        case eitherDecode "{\"programIdIndex\":0,\"accounts\":[0,1],\"data\":\"0OIl\"}" :: Either String CompiledInstruction of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure on invalid base58"
```

(`0`, `O`, `I`, `l` are excluded from the base58 alphabet, so decoding must fail.)

- [ ] **Step 2: Run and verify it fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL — the current instance's `fromJust` produces a `Right` with a delayed crash inside, so the test's `Right` branch triggers `assertFailure`.

- [ ] **Step 3: Fix the instance**

In `src/Network/Solana/Core/Instruction.hs`, replace the `FromJSON CompiledInstruction` instance:

```haskell
instance FromJSON CompiledInstruction where
  parseJSON :: Value -> Parser CompiledInstruction
  parseJSON = withObject "CompiledInstruction" $ \v -> do
    programIdIndex <- v .: "programIdIndex"
    accounts <- v .: "accounts"
    dataStr <- v .: "data"
    bytes <- case fromBase58String dataStr of
      Nothing -> fail "CompiledInstruction: data is not valid base58"
      Just bs -> pure bs
    pure
      CompiledInstruction
        { ciProgramIdIndex = programIdIndex,
          ciAccounts = mkCompact accounts,
          ciData = mkCompact (S.unpack bytes)
        }
```

Then remove the now-unused `Data.Maybe (fromJust)` import from this file (it becomes an orphan of this change).

- [ ] **Step 4: Run and verify all pass**

Run: `cabal test --test-show-details=direct`
Expected: PASS, and `cabal build all` stays warning-clean.

- [ ] **Step 5: Commit**

```bash
git add src/Network/Solana/Core/Instruction.hs test/Test/Core/Instruction.hs
git commit -m "fix: CompiledInstruction JSON parsing fails cleanly on invalid base58"
```

---

### Task 11: GitHub Actions CI

**Files:**
- Create: `.github/workflows/ci.yml`

**Interfaces:**
- Produces: CI running build + tests on pushes to `main`/`development` and on PRs.

- [ ] **Step 1: Write the workflow**

Create `.github/workflows/ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main, development]
  pull_request:

jobs:
  build-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: "9.6"
          cabal-version: "3.10"
      - name: Freeze dependencies
        run: cabal freeze
      - uses: actions/cache@v4
        with:
          path: |
            ${{ steps.setup.outputs.cabal-store }}
            dist-newstyle
          key: ${{ runner.os }}-ghc-9.6-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: ${{ runner.os }}-ghc-9.6-
      - name: Build
        run: cabal build all
      - name: Test
        run: cabal test all --test-show-details=direct
```

- [ ] **Step 2: Validate locally as far as possible**

Run: `cabal build all && cabal test all --test-show-details=direct`
Expected: PASS (same commands CI runs). The workflow itself can only be verified after a push; if the remote build fails on the executable's `web3-provider` dependency, narrow the build step to `cabal build lib:solana-haskell-sdk solana-haskell-sdk-test` — but try `all` first.

- [ ] **Step 3: Commit**

```bash
git add .github/workflows/ci.yml
git commit -m "ci: build and test on GitHub Actions"
```

---

### Task 12: README + CHANGELOG accuracy

**Files:**
- Modify: `README.md`
- Modify: `CHANGELOG.md`

**Interfaces:**
- Consumes: the now-true state of the codebase (all 13 System instructions, tests, CI).
- Produces: documentation matching reality; roadmap matching the approved spec.

- [ ] **Step 1: Update the README**

In `README.md`:

1. Fix the broken badge link `https://https://hackage.haskell.org/` → `https://hackage.haskell.org/`.
2. Replace the `## Features` checklist with:

```markdown
## Features

- [x] [Full JSON RPC API](#usage-examples-for-json-rpc-api)
- [x] Wallet, account and keys management
- [x] Transaction Building
  - [ ] Clients for native programs
    - [x] System Program (all 13 instructions)
    - [ ] Compute Budget
    - [ ] Memo
    - [ ] Stake
    - [ ] Vote
    - [ ] BPF Loader
    - [ ] Secp256k1
  - [ ] Clients for Solana Program Library (SPL)
    - [ ] SPL Token
    - [ ] Associated Token Account

### Post-1.0

- Metaplex (Token Metadata; requires Borsh serialization)
- Versioned (v0) transactions and Address Lookup Tables
```

3. In the `### Current development status` section, add one sentence at the end of the existing paragraph: `Serialization is verified byte-for-byte against the official Rust SDK by golden-vector tests (see test/fixtures/).`

- [ ] **Step 2: Update the CHANGELOG**

Replace the contents of `CHANGELOG.md` with:

```markdown
# Revision history for solana-haskell-sdk

## Unreleased

* Test suite: golden vectors generated from the Rust `solana-sdk` (byte-for-byte
  serialization checks at instruction, message, and signed-transaction level),
  plus QuickCheck properties for compact-u16, crypto round-trips, and message
  header/key-ordering invariants.
* Fixed: pubkeys inside System Program instruction data were serialized with a
  length prefix (invalid wire format).
* Fixed: `CreateAccountWithSeed` seed was missing its u64 length prefix.
* Fixed: `SolanaSignature` binary decoding read 32 bytes instead of 64.
* Fixed: the program id was appended as an extra account to every instruction;
  it is now added only to the message account keys.
* Fixed: compact-u16 decoding now rejects aliased (non-canonical) encodings.
* Fixed: `CompiledInstruction` JSON parsing fails cleanly on invalid base58.
* Added the 9 remaining System Program instructions: nonce account management
  (advance, withdraw, initialize, authorize, upgrade) and
  allocate/assign/transfer with seed.
* Added CI (GitHub Actions).

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
```

- [ ] **Step 3: Verify and commit**

Run: `cabal test --test-show-details=direct`
Expected: PASS (unchanged — docs only).

```bash
git add README.md CHANGELOG.md
git commit -m "docs: accurate roadmap, changelog for hardening work"
```

---

## Success criteria (from the spec)

1. `cabal test` passes locally and in GitHub Actions — Tasks 1-11.
2. Every `SystemInstruction` variant byte-identical to Rust golden vectors — Tasks 5, 8, 9.
3. Signed transfer transaction byte-identical to Rust golden vector — Task 6.
4. All 13 System Program instructions implemented with builders and tests — Tasks 5, 8, 9.
5. README roadmap reflects reality — Task 12.
