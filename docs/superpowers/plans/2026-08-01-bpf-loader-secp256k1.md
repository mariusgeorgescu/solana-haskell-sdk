# BPF Loader Upgradeable + Secp256k1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byte-verified upgradeable-loader and secp256k1-precompile clients — the last two roadmap programs.

**Architecture:** Golden-vector pattern. Loader is bincode (u32 LE disc) with one Vec<u8> field (u64 LE length + raw bytes); builders follow `solana_sdk::bpf_loader_upgradeable`; `programDataAddress` reuses `Core.Pda`. Secp256k1 is a precompile: no accounts, hand-packed offsets struct; the fixture is produced by Rust's OWN signer (`new_secp256k1_instruction` + libsecp256k1) and the Haskell builder must reconstruct it byte-identically from the emitted components.

**Tech Stack:** Haskell (GHC 9.6.7, tasty/QuickCheck), Rust (solana-sdk 1.18 + libsecp256k1) for fixtures only.

**Spec:** `docs/superpowers/specs/2026-08-01-bpf-loader-secp256k1-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push. `cabal build all` warning-clean (`-Wall`). **No new Haskell dependencies.**
- Fixtures: additions only; existing entries byte-identical. CI never runs cargo.
- Fixed constants (generator and Haskell tests MUST match):
  - buffer pubkey 32×24; program pubkey 32×23; spill/recipient pubkey 32×22 (reuse); authority = payer pubkey (seed 32×1); new-authority pubkey 32×20 (reuse)
  - Write offset `128`, Write bytes = 64 bytes of `0xAB`; DeployWithMaxDataLen `1048576`; ExtendProgram `4096`
  - secp secret key = 32 bytes of `42`; secp message = `"hello-secp"` (UTF-8)
- Program ids: loader `BPFLoaderUpgradeab1e11111111111111111111111`; secp256k1 `KeccakSecp256k11111111111111111111111111111`.
- Wire: loader u32 LE disc 0-7; Vec<u8> = u64 LE length + bytes. Secp instruction data = count u8 (=1) + 11-byte offsets (u16 LE sigOffset=32, u8 sigIxIndex=0, u16 ethOffset=12, u8 ethIxIndex=0, u16 msgOffset=97, u16 msgSize, u8 msgIxIndex=0) + eth 20B + sig 64B + recovery u8 + message. If Rust-emitted bytes contradict any of this, THE FIXTURE WINS — adapt and note.
- Match existing code style (Stake.hs is the closest template).

---

### Task 1: Fixture generator — loader and secp vectors

**Files:**
- Modify: `tools/fixture-gen/Cargo.toml` (add `libsecp256k1 = "0.6"`), `tools/fixture-gen/src/main.rs`
- Create (generated): `test/fixtures/loader_instruction_data.json`, `test/fixtures/secp256k1.json`
- Modify (generated): `test/fixtures/pda.json` (one appended entry `programdata-address`)

**Interfaces:**
- Produces loader entries: `InitializeBuffer`, `Write`, `DeployWithMaxDataLen`, `Upgrade`, `SetAuthority`, `Close`, `ExtendProgram`, `SetAuthorityChecked`; secp entries: `secp-full-instruction-data`, `secp-eth-address` (20B), `secp-signature` (64B), `secp-recovery-id` (1B), `secp-message`; pda entry: `programdata-address`.

- [ ] **Step 1: Extend the generator.** Imports:

```rust
use solana_sdk::loader_upgradeable_instruction::UpgradeableLoaderInstruction;
use solana_sdk::secp256k1_instruction::new_secp256k1_instruction;
```

Append at end of `main()`:

```rust
    let loader_variants: Vec<(&str, UpgradeableLoaderInstruction)> = vec![
        ("InitializeBuffer", UpgradeableLoaderInstruction::InitializeBuffer),
        ("Write", UpgradeableLoaderInstruction::Write { offset: 128, bytes: vec![0xAB; 64] }),
        ("DeployWithMaxDataLen", UpgradeableLoaderInstruction::DeployWithMaxDataLen { max_data_len: 1_048_576 }),
        ("Upgrade", UpgradeableLoaderInstruction::Upgrade),
        ("SetAuthority", UpgradeableLoaderInstruction::SetAuthority),
        ("Close", UpgradeableLoaderInstruction::Close),
        ("ExtendProgram", UpgradeableLoaderInstruction::ExtendProgram { additional_bytes: 4096 }),
        ("SetAuthorityChecked", UpgradeableLoaderInstruction::SetAuthorityChecked),
    ];
    let loader_fixtures: Vec<_> = loader_variants
        .iter()
        .map(|(name, v)| json!({ "name": name, "hex": hex(&bincode::serialize(v).unwrap()) }))
        .collect();
    fs::write(
        "../../test/fixtures/loader_instruction_data.json",
        serde_json::to_string_pretty(&loader_fixtures).unwrap(),
    )
    .unwrap();

    let program_id_fixed = Pubkey::new_from_array([23u8; 32]);
    let (programdata, _pd_bump) = Pubkey::find_program_address(
        &[program_id_fixed.as_ref()],
        &solana_sdk::bpf_loader_upgradeable::id(),
    );
    // append to the EXISTING pda_fixtures write or emit via read-modify-write:
    // simplest correct approach — rebuild pda.json with the original five entries
    // plus this one appended, i.e. add to the pda_fixtures vec BEFORE its fs::write:
    //     json!({ "name": "programdata-address", "hex": hex(&programdata.to_bytes()) }),
    // (move this block above the pda.json fs::write and keep entry order: existing five, then this one)

    let secp_secret = libsecp256k1::SecretKey::parse(&[42u8; 32]).unwrap();
    let secp_ix = new_secp256k1_instruction(&secp_secret, b"hello-secp");
    let data = &secp_ix.data;
    // layout: [0]=count, [1..12]=offsets, [12..32]=eth, [32..96]=sig, [96]=recovery, [97..]=msg
    let secp_fixtures = vec![
        json!({ "name": "secp-full-instruction-data", "hex": hex(data) }),
        json!({ "name": "secp-eth-address", "hex": hex(&data[12..32]) }),
        json!({ "name": "secp-signature", "hex": hex(&data[32..96]) }),
        json!({ "name": "secp-recovery-id", "hex": hex(&data[96..97]) }),
        json!({ "name": "secp-message", "hex": hex(&data[97..]) }),
    ];
    fs::write(
        "../../test/fixtures/secp256k1.json",
        serde_json::to_string_pretty(&secp_fixtures).unwrap(),
    )
    .unwrap();
```

Cargo.toml: add `libsecp256k1 = "0.6"` (match the version solana-sdk 1.18 uses — check `cargo tree -i libsecp256k1` after adding; if solana-sdk pins 0.6.x this unifies, otherwise match its major).

The `programdata-address` entry MUST be appended to the existing `pda_fixtures` vec (before its `fs::write`) so pda.json keeps its five existing entries unchanged, with the new one last.

- [ ] **Step 2: Regenerate; verify** — additions-only on pda.json; 8 loader entries; secp-full length = 97 + 10 (message "hello-secp" = 10 bytes) = 107 bytes → 214 hex chars; assert `data[0] == 0x01`; suite still 148 green.
- [ ] **Step 3: Commit** (`git commit -m "test: golden vectors for upgradeable loader and secp256k1 precompile"`, files: tools/fixture-gen/, test/fixtures/).

---

### Task 2: BpfLoaderUpgradeable module

**Files:**
- Create: `src/Network/Solana/NativePrograms/BpfLoaderUpgradeable.hs`, `test/Test/NativePrograms/BpfLoaderUpgradeable.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`

**Interfaces:**
- Produces: `bpfLoaderUpgradeableId :: SolanaPublicKey`; `data UpgradeableLoaderInstruction = InitializeBuffer | Write {wOffset :: Word32, wBytes :: BS.ByteString} | DeployWithMaxDataLen {dMaxDataLen :: Word64} | Upgrade | SetAuthority | Close | ExtendProgram {eAdditionalBytes :: Word32} | SetAuthorityChecked` (symmetric Binary: u32 LE disc; Write's bytes as u64 LE length + raw, with a maxBound-Int length guard on get); `programDataAddress :: SolanaPublicKey -> Maybe SolanaPublicKey`; builders per the meta table below.

Builder signatures + metas (Rust `bpf_loader_upgradeable` ground truth):

```haskell
initializeBuffer :: SolanaPublicKey -> SolanaPublicKey -> Instruction
-- buffer, authority: [w buffer, ro authority]; payload InitializeBuffer
write :: SolanaPublicKey -> SolanaPublicKey -> Word32 -> BS.ByteString -> Instruction
-- buffer, authority, offset, bytes: [w buffer, s authority]; payload Write
deployWithMaxDataLen :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction
-- payer, programdata, program, buffer, authority, maxDataLen:
-- [ws payer, w programdata, w program, w buffer, ro rent, ro clock, ro systemProgramId, s authority]
upgrade :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
-- programdata, program, buffer, spill, authority:
-- [w programdata, w program, w buffer, w spill, ro rent, ro clock, s authority]
setAuthority :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
-- owned, currentAuthority, newAuthority: [w owned, s current, ro new]; payload SetAuthority
setAuthorityChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
-- owned, currentAuthority, newAuthority: [w owned, s current, s new]; payload SetAuthorityChecked
closeAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey -> Instruction
-- account, recipient, authority, optional associated program (for programdata closes):
-- [w account, w recipient, s authority] <> maybe [] (\p -> [w p]) mProgram; payload Close
extendProgram :: SolanaPublicKey -> SolanaPublicKey -> Maybe (SolanaPublicKey, SolanaPublicKey) -> Word32 -> Instruction
-- programdata, program, optional (payer with system program), additionalBytes:
-- [w programdata, w program] <> maybe [] (\(sys, payer) -> [ro sys, ws payer]) mPayer; payload ExtendProgram
-- (Rust: extend_program(program_data, program, Some(payer)) appends [ro system_program, ws payer])
```

`programDataAddress prog = fst <$> findProgramAddress [getSolanaPublicKeyRaw prog] bpfLoaderUpgradeableId`.

- [ ] **Step 1: Tests** — 8 goldens (Write with offset 128 / 64×0xAB bytes; DeployWithMaxDataLen 1048576; ExtendProgram 4096); round-trip property over all 8 constructors (Write bytes via `BS.pack <$> listOf arbitrary`); unknown-disc `[8,0,0,0]` → Left; `programdata-address` golden (`programDataAddress (32×23 pubkey)` vs pda.json fixture); meta tests for all 8 builders using constants bufferPk 32×24, programPk 32×23, spillPk 32×22, newAuthPk 32×20, payerPk (seed 1), `SP.systemProgramId`, `Sysvar.rent`, `Sysvar.clock` — including BOTH Maybe arms for closeAccount and extendProgram. Structure the test module like Test.NativePrograms.Stake.
- [ ] **Step 2: Register** (cabal: BpfLoaderUpgradeable first among NativePrograms alphabetically — before ComputeBudget; test module likewise; wire test/Main.hs), compile failure.
- [ ] **Step 3: Implement** (template: Stake.hs; the Vec<u8> get needs the length guard: `len <- getWord64le; if len > fromIntegral (maxBound :: Int) then fail ... else getByteString (fromIntegral len)`).
- [ ] **Step 4: Green** (expect 148 + 8 + 1 + 1 + 1 + 10 = 169; count in report). **Step 5: Commit** (`feat: upgradeable BPF loader client`, four files).

---

### Task 3: Secp256k1 module + docs

**Files:**
- Create: `src/Network/Solana/NativePrograms/Secp256k1.hs`, `test/Test/NativePrograms/Secp256k1.hs`
- Modify: `solana-haskell-sdk.cabal`, `test/Main.hs`, `README.md`, `CHANGELOG.md`

**Interfaces:**
- Produces: `secp256k1ProgramId :: SolanaPublicKey`; `data SecpSignatureOffsets = SecpSignatureOffsets {ssoSignatureOffset :: Word16, ssoSignatureInstructionIndex :: Word8, ssoEthAddressOffset :: Word16, ssoEthAddressInstructionIndex :: Word8, ssoMessageDataOffset :: Word16, ssoMessageDataSize :: Word16, ssoMessageInstructionIndex :: Word8}` (symmetric Binary, all LE); `newSecp256k1Instruction :: BS.ByteString -> BS.ByteString -> Word8 -> BS.ByteString -> Instruction` (ethAddress, signature, recoveryId, message) — `error` with a clear message if ethAddress /= 20 bytes or signature /= 64 bytes (documented in Haddock; mirrors the Rust builder's assertions); no accounts.

Data layout (must reproduce Rust exactly): count u8 = 1; offsets with ethAddressOffset = 12, signatureOffset = 32, messageDataOffset = 97, messageDataSize = length message, all instruction indexes = 0; then ethAddress <> signature <> recoveryId <> message. Implement via the Binary instance of SecpSignatureOffsets + `Data.Binary.Put` composition; the instruction's data is that byte string (use `mkInstruction secp256k1ProgramId []` with a newtype wrapper OR construct `Instruction` directly with `InstructionData` — pick whichever matches `mkInstruction`'s Binary-based API; a small `newtype Secp256k1InstructionData = Secp256k1InstructionData BS.ByteString` with a raw-put Binary instance mirrors Memo's `MemoData` pattern).

- [ ] **Step 1: Tests** — from `test/fixtures/secp256k1.json`: reconstruct via `newSecp256k1Instruction eth sig recovery msg` (components from fixtures) and assert `instrData (iData ix) == secp-full-instruction-data` fixture; offsets golden: decode bytes 1..12 of the full fixture as SecpSignatureOffsets and assert the expected field values (32, 0, 12, 0, 97, 10, 0); round-trip property for SecpSignatureOffsets; length-validation: 19-byte eth address and 63-byte signature must throw (use `evaluate` + `try`/`ErrorCall` or tasty-hunit's assertion on `Control.Exception.try` — mirror any existing error-testing pattern, or use QuickCheck's `ioProperty` with `catch`); `iAccounts ix == []`.
- [ ] **Step 2: Register (alphabetical: after Memo? No — NativePrograms: BpfLoaderUpgradeable < ComputeBudget < Secp256k1 < Stake < SystemProgram < Vote), compile failure. Step 3: Implement. Step 4: Green.**
- [ ] **Step 5: Docs** — README: `- [x] BPF Loader (upgradeable)` and `- [x] Secp256k1 (instruction construction; signing out of scope)` (replace the two unchecked entries; keep list order); ALSO add the phase-4 carry-forward subset qualifiers: change `- [x] Stake` to `- [x] Stake (delegation lifecycle; seed-authority variants post-1.0)` and `- [x] Vote` to `- [x] Vote (account management)`. CHANGELOG top of Unreleased:

```markdown
* Added upgradeable BPF loader client (buffer/write/deploy/upgrade/authority/
  close/extend) with program-data address derivation.
* Added secp256k1 precompile client: signature-verification instruction
  construction from a precomputed signature (signing is out of scope).
```

- [ ] **Step 6: Full suite green; commit** (`feat: secp256k1 precompile client; roadmap and changelog for loader & secp256k1`, six files).

---

## Success criteria (from the spec)

1. Suite green — every task. 2. Loader + secp goldens — Tasks 1-3. 3. programDataAddress golden — Tasks 1-2. 4. Docs — Task 3.
