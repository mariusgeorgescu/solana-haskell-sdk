# Versioned (v0) Transactions + Address Lookup Tables — Design

**Date:** 2026-08-01
**Status:** Approved (post-1.0 sub-project A; autonomous-goal mode)
**Goal:** Byte-verified v0 message compilation/signing and an Address Lookup Table program client — lifting the legacy-only limitation recorded since phase 1.

## Context

The SDK (1.0.0.0-ready, 187 tests) builds only legacy messages. v0 messages shrink transactions by loading non-signer accounts from on-chain lookup tables. Ground truth remains the Rust SDK via fixture-gen. Work on `development`; never push.

## Scope

### Network.Solana.NativePrograms.AddressLookupTable (new)

- `addressLookupTableProgramId = "AddressLookupTab1e1111111111111111111111111"`.
- `data AddressLookupTableInstruction` (bincode u32 LE discriminants): CreateLookupTable {recentSlot :: Word64, bumpSeed :: Word8} = 0, FreezeLookupTable = 1, ExtendLookupTable {newAddresses :: [SolanaPublicKey]} = 2 (bincode Vec: u64 LE count + raw 32B each), DeactivateLookupTable = 3, CloseLookupTable = 4. Symmetric Binary.
- `deriveLookupTableAddress :: SolanaPublicKey -> Word64 -> Maybe (SolanaPublicKey, Word8)` — PDA of [authority, recentSlot as 8 LE bytes] under the program.
- Builders mirroring `solana_sdk::address_lookup_table::instruction`: `createLookupTable` (authority NON-signer variant) returning `(Instruction, SolanaPublicKey)` like Rust (instruction + derived table address); `freezeLookupTable`, `extendLookupTable` (with `Maybe payer` — Just appends [ws payer, ro system]), `deactivateLookupTable`, `closeLookupTable`. Metas per the Rust source.

### v0 message support (Network.Solana.Core.VersionedMessage — new module)

- `data AddressLookupTableAccount = AddressLookupTableAccount {altKey :: SolanaPublicKey, altAddresses :: [SolanaPublicKey]}` — the client-side view of a table's contents (fetched off-chain by the caller; account-state parsing is out of scope).
- `data MessageAddressTableLookup = MessageAddressTableLookup {mtlAccountKey :: SolanaPublicKey, mtlWritableIndexes :: [Word8], mtlReadonlyIndexes :: [Word8]}` — wire form: key 32B + compact array u8 + compact array u8.
- V0 wire format: prefix byte 0x80 (version-0 tag), then header (3 bytes), compact static keys, blockhash, compact compiled instructions, compact table-lookups.
- `compileV0Message :: BlockHash -> [Instruction] -> [AddressLookupTableAccount] -> Either CompileException ByteString` — mirrors Rust `Message::try_compile`: signers and PROGRAM IDS stay static (programs cannot load from tables); non-signer keys found in a table (first match wins, in table order) move to that table's lookup (writable vs readonly per privilege union); statics keep canonical ordering; instruction indices span statics ++ concat(writable lookups) ++ concat(readonly lookups) in table order.
- `newV0TransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> [AddressLookupTableAccount] -> BlockHash -> Either CompileException String` — compact signatures + v0 message, base64 (mirrors `newTransactionIntent`).
- The legacy path is untouched.

## Testing

fixture-gen additions (Rust `solana_sdk::address_lookup_table` + `message::v0::Message::try_compile` + `VersionedTransaction::try_new`):
- `alt_instruction_data.json`: 5 variants (CreateLookupTable {recentSlot 12345, bump from derivation}, Extend with 2 fixed addresses 32×29/32×30, others field-less).
- `pda.json` += `lookup-table-address` + `lookup-table-bump` (authority = payer, recentSlot 12345; assert bump < 255 if possible, else document).
- `transactions.json` += `v0-transfer-transaction`: transfer payer→recipient(32×2) where the recipient and a second transfer's recipient (32×29) load from a lookup table (table key 32×31, addresses [32×2, 32×29, 32×30]), signed by payer, blockhash 32×9. Also `v0-message` (unsigned bytes of the same message).
- Haskell: goldens per ALT variant + PDA + v0 message + v0 transaction byte-identity; meta tests per builder; round-trip property for the ALT instruction type; unknown-discriminant test; compile-behavior unit tests (program id never in lookups; signer never in lookups; key not in any table stays static).

## Out of scope

Parsing v0 messages/transactions from the wire (encode-only compile path, like the rest of the SDK's tx building); fetching/deserializing lookup-table ACCOUNT STATE from RPC; RPC plumbing for v0 (getBlock v0 parse limitation stands as documented).

## Success criteria

1. Suite green with all new tests. 2. ALT instruction + PDA goldens byte-identical. 3. v0 message AND signed v0 transaction byte-identical to Rust. 4. Docs updated (README feature list; CHANGELOG Unreleased section).
