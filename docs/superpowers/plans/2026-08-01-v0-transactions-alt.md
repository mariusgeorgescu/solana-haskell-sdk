# v0 Transactions + Address Lookup Tables Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Byte-verified v0 message/transaction building + ALT program client.

**Architecture:** ALT client follows the established bincode pattern (Stake.hs template). v0 support is a NEW module `Network.Solana.Core.VersionedMessage` reusing the legacy compiler's bucket/canonicalization machinery conceptually but implementing its own static-vs-lookup partition mirroring Rust `message::v0::Message::try_compile`. Encode-only. Rust fixtures arbitrate everything.

**Spec:** `docs/superpowers/specs/2026-08-01-v0-transactions-alt-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push. `cabal build all` -Wall clean; hlint clean. **No new Haskell dependencies.** Fixtures additions-only. CI never runs cargo.
- Fixed constants: recentSlot `12345`; lookup-table authority = payer (seed 32×1); table key for the tx fixture: 32×31; table addresses `[32×2, 32×29, 32×30]`; second recipient 32×29; extend-addresses `[32×29, 32×30]`; transfer amounts 1000000000 (to 32×2) and 500000 (to 32×29); blockhash 32×9.
- ALT program id `AddressLookupTab1e1111111111111111111111111`. Discriminants u32 LE 0-4 as in the spec. bincode Vec<Pubkey> = u64 LE count + raw 32B each.
- v0 wire: 0x80 prefix, header, compact statics, blockhash, compact instructions, compact lookups (key + compact u8 idx + compact u8 idx). Program ids and signers NEVER in lookups. THE FIXTURE WINS on any disagreement.
- Style: existing module conventions (Stake.hs, Message.hs).

---

### Task 1: Fixtures — ALT vectors, lookup PDA, v0 message + transaction

**Files:** Modify `tools/fixture-gen/src/main.rs`; generated: `test/fixtures/alt_instruction_data.json` (new), `pda.json` += 2 entries, `transactions.json` += 2 entries (`v0-message`, `v0-transfer-transaction`).

- [ ] **Step 1:** Add imports: `use solana_sdk::address_lookup_table::{self, instruction::ProgramInstruction, AddressLookupTableAccount};` and `use solana_sdk::message::{v0, VersionedMessage};` and `use solana_sdk::transaction::VersionedTransaction;` (adapt paths to 1.18 as needed — e.g. `solana_sdk::address_lookup_table::instruction::ProgramInstruction` may be `solana_program::address_lookup_table::instruction::ProgramInstruction`; document adaptations).
- [ ] **Step 2:** Append to `main()`:

```rust
    let (lut_addr, lut_bump) =
        Pubkey::find_program_address(&[payer.pubkey().as_ref(), &12345u64.to_le_bytes()], &address_lookup_table::program::id());
    let alt_addr_a = Pubkey::new_from_array([29u8; 32]);
    let alt_addr_b = Pubkey::new_from_array([30u8; 32]);
    let alt_variants: Vec<(&str, ProgramInstruction)> = vec![
        ("CreateLookupTable", ProgramInstruction::CreateLookupTable { recent_slot: 12345, bump_seed: lut_bump }),
        ("FreezeLookupTable", ProgramInstruction::FreezeLookupTable),
        ("ExtendLookupTable", ProgramInstruction::ExtendLookupTable { new_addresses: vec![alt_addr_a, alt_addr_b] }),
        ("DeactivateLookupTable", ProgramInstruction::DeactivateLookupTable),
        ("CloseLookupTable", ProgramInstruction::CloseLookupTable),
    ];
```

emit as `alt_instruction_data.json` (same json!/hex pattern; bincode::serialize). Append to `pda_fixtures` (before its write): `lookup-table-address` = lut_addr bytes, `lookup-table-bump` = [lut_bump]. NOTE: if `ProgramInstruction` is not exposed in 1.18's sdk, construct via `instruction::create_lookup_table/...` and use each returned `Instruction.data` instead (same bytes) — document which path.

- [ ] **Step 3:** v0 fixtures:

```rust
    let lut_account = AddressLookupTableAccount {
        key: Pubkey::new_from_array([31u8; 32]),
        addresses: vec![recipient, alt_addr_a, alt_addr_b],
    };
    let v0_msg = v0::Message::try_compile(
        &payer.pubkey(),
        &[
            system_instruction::transfer(&payer.pubkey(), &recipient, 1_000_000_000),
            system_instruction::transfer(&payer.pubkey(), &alt_addr_a, 500_000),
        ],
        &[lut_account],
        blockhash,
    ).unwrap();
    let versioned = VersionedMessage::V0(v0_msg);
    let v0_tx = VersionedTransaction::try_new(versioned.clone(), &[&payer]).unwrap();
```

Append to `tx_fixtures`: `v0-message` = hex(versioned.serialize()), `v0-transfer-transaction` = hex(bincode::serialize(&v0_tx)). (VersionedMessage::serialize exists; if not, `bincode::serialize(&versioned)` — the fixture arbitrates; document.)

- [ ] **Step 4:** `cargo run`; verify: additions-only on pda.json/transactions.json; 5 alt entries; `v0-message` hex starts `80`; `CreateLookupTable` hex starts `00000000` + `3930000000000000` (12345 LE); suite still 187 green. Record lut_bump value in the report (Haskell tests need the derivation to match, and the create builder golden embeds it).
- [ ] **Step 5:** Commit `test: golden vectors for address lookup tables and v0 transactions` (tools/fixture-gen/ + test/fixtures/).

---

### Task 2: ALT program client

**Files:** Create `src/Network/Solana/NativePrograms/AddressLookupTable.hs`, `test/Test/NativePrograms/AddressLookupTable.hs`; modify cabal + test/Main.hs.

**Interfaces (produces):** `addressLookupTableProgramId`; `data AddressLookupTableInstruction = CreateLookupTable {recentSlot :: Word64, bumpSeed :: Word8} | FreezeLookupTable | ExtendLookupTable {newAddresses :: [SolanaPublicKey]} | DeactivateLookupTable | CloseLookupTable` (u32 LE disc 0-4; Vec<Pubkey> = u64 LE count + raw keys; symmetric Binary with count guard like getBincodeString's); `deriveLookupTableAddress :: SolanaPublicKey -> Word64 -> Maybe (SolanaPublicKey, Word8)` (PDA [authority raw, 8-byte LE slot]); builders:

```haskell
createLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Word64 -> (Instruction, SolanaPublicKey)
-- authority, payer, recentSlot; derives (table, bump); metas [w table, ro authority, ws payer, ro SP.systemProgramId]; payload CreateLookupTable recentSlot bump; returns (ix, table)
freezeLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Instruction   -- table, authority: [w table, s authority]
extendLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey -> [SolanaPublicKey] -> Instruction
-- table, authority, mPayer, newAddresses: [w table, s authority] <> maybe [] (\p -> [ws p, ro SP.systemProgramId]) mPayer
deactivateLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Instruction -- [w table, s authority]
closeLookupTable :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction -- table, authority, recipient: [w table, s authority, w recipient]
```

(`createLookupTable`'s derivation failure: `error` with clear message, mirroring ataMetas precedent, Haddock-documented.) Word64→LE bytes: `BL.toStrict (runPut (putWord64le slot))` or Data.ByteString.Builder — implementer's choice, tested by the PDA golden.

- [ ] Steps: TDD — tests (5 goldens incl. CreateLookupTable using the bump from `deriveLookupTableAddress` (assert equality with `lookup-table-bump` fixture first), PDA golden vs `lookup-table-address`, round-trip property, unknown-disc [5,0,0,0], meta tests for all 5 builders incl. both extend arms; constants: authority=payerPk seed 1, tablePk placeholder 32×31 for non-create builders, addrs 32×29/30, recipient 32×22) → register (alphabetical: AddressLookupTable FIRST among NativePrograms) → compile fail → implement (Stake.hs template) → green (expect 187 + ~13) → commit `feat: address lookup table program client`.

---

### Task 3: v0 message compilation + signing

**Files:** Create `src/Network/Solana/Core/VersionedMessage.hs`, `test/Test/Core/VersionedMessage.hs`; modify cabal + test/Main.hs. (Do NOT modify Message.hs — reuse via imports where exports allow; Message.hs already exports Message(..), MessageHeader(..), mkNewMessage.)

**Interfaces (produces):**

```haskell
data AddressLookupTableAccount = AddressLookupTableAccount {altKey :: SolanaPublicKey, altAddresses :: [SolanaPublicKey]}
data MessageAddressTableLookup = MessageAddressTableLookup {mtlAccountKey :: SolanaPublicKey, mtlWritableIndexes :: [Word8], mtlReadonlyIndexes :: [Word8]}
compileV0Message :: BlockHash -> [Instruction] -> [AddressLookupTableAccount] -> Either CompileException BS.ByteString
newV0TransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> [AddressLookupTableAccount] -> BlockHash -> Either CompileException String
```

**Algorithm (mirror Rust v0::Message::try_compile — the fixture arbitrates):**
1. Build the legacy Message via `mkNewMessage` (privilege union + canonical ordering already done): gives header + ordered keys (ws / ro-signed / w-unsigned / ro-unsigned sections via `splitAccountsByPurpose` — NOT exported; recompute the split locally from the header counts, which IS possible from Message(..) exports: nrs/nros/nrou + key list).
2. Collect the set of PROGRAM IDS (iProgramId of each instruction) — these must stay static.
3. For each lookup table IN ORDER: from the writable-unsigned section, keys present in that table's addresses AND not program ids move to that table's writable indexes (index = position in altAddresses); from readonly-unsigned likewise to readonly indexes. First table containing a key wins. Preserve the section-relative order when draining (Rust drains in the order the keys appear in the static list).
4. Remaining static keys: ws ++ ro-signed ++ remaining w-unsigned ++ remaining ro-unsigned; header counts adjust: numReadonlyUnsigned = remaining ro-unsigned count (writable-signed etc. unchanged).
5. Instruction account indices resolve over: statics ++ (for each table in order: its writable-loaded keys) ++ (for each table in order: its readonly-loaded keys). Program id index over statics only.
6. Serialize: putWord8 0x80, header (3×Word8), compact statics (CompactArray SolanaPublicKey), blockhash raw 32, compact CompiledInstructions (reuse compileInstruction with the FULL resolution key list — check its signature takes [SolanaPublicKey]; it does), compact lookups (each: raw key, compact Word8 list ×2). Tables with no loaded keys are OMITTED from the lookups array (Rust omits empty lookups).
7. newV0TransactionIntent: sign the message bytes with dsign per signer, compact signature array ++ message, base64 (mirror newTransactionIntent's implementation).

- [ ] Steps: TDD — goldens first (`v0-message` via compileV0Message with [transfer payer→recipientPk 1000000000, transfer payer→addr29 500000] + the 32×31 table [recipient, 29, 30]; `v0-transfer-transaction` via newV0TransactionIntent [payerPriv]), plus unit tests: program ids stay static even if in a table (build a table containing systemProgramId — compile a transfer; assert lookups don't contain it — assert via... expose the intermediate? Simplest: a table containing ONLY systemProgramId must produce a message with NO lookups → bytes must equal a v0 compile with NO tables); signer stays static (table containing payer → same no-lookup result); key-absent-from-tables stays static. Run RED (module missing) → implement → GREEN → commit `feat: v0 message compilation and versioned transaction signing`.
- If the golden mismatches, hex-diff and check: prefix byte, lookup drain order, empty-lookup omission, index resolution order (statics, then writable per table, then readonly per table). The fixture wins. BLOCKED report if not resolvable by aligning to Rust's algorithm.

---

### Task 4: Docs

**Files:** README.md, CHANGELOG.md.

- [ ] README: under Features, add `- [x] Versioned (v0) transactions and Address Lookup Tables` (native-programs list gains `- [x] Address Lookup Table`); move the item out of the Post-1.0 section (leave Metaplex there). CHANGELOG: add a `## Unreleased` section above 1.0.0.0 with: `* Added Address Lookup Table program client and versioned (v0) transaction support: compile messages against lookup tables and sign VersionedTransactions (encode-only; account-state parsing out of scope).`
- [ ] Suite green; commit `docs: v0 transactions and address lookup tables`.

## Success criteria

1. Suite green — all tasks. 2. ALT + PDA goldens — Tasks 1-2. 3. v0 message + tx byte-identity — Tasks 1, 3. 4. Docs — Task 4.
