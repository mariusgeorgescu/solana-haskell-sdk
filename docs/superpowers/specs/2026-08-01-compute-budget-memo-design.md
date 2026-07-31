# Compute Budget + Memo Clients — Design

**Date:** 2026-08-01
**Status:** Approved (sub-project 2 of 6; roadmap in `2026-07-31-core-verification-hardening-design.md`)
**Goal:** Add byte-verified clients for the Compute Budget program (priority fees) and the SPL Memo program, proving the "add a program client" pattern on the phase-1 hardened core.

## Context

Phase 1 (merged to `main` as `c8f6c07`) delivered golden-vector testing against the official Rust `solana-sdk`, a corrected wire format, all 13 System Program instructions, and CI. Phase 2 is deliberately small: two high-value programs whose clients exercise the established pattern end-to-end, plus one piece of debt the phase-1 final review flagged.

Work happens on `development` (synced with `main`); pushes remain the maintainer's.

## Scope

### Network.Solana.NativePrograms.ComputeBudget (new)

- `computeBudgetProgramId :: SolanaPublicKey` = `ComputeBudget111111111111111111111111111111`.
- `data ComputeBudgetInstruction`, serialized in the Rust SDK's borsh layout — **u8 discriminant + little-endian field** (unlike System Program's bincode u32 discriminant):
  - `RequestHeapFrame Word32` (discriminant 1)
  - `SetComputeUnitLimit Word32` (2)
  - `SetComputeUnitPrice Word64` (3, micro-lamports)
  - `SetLoadedAccountsDataSizeLimit Word32` (4)
  - The deprecated variant 0 (`Unused`) is intentionally not modeled.
- Builders `requestHeapFrame`, `setComputeUnitLimit`, `setComputeUnitPrice`, `setLoadedAccountsDataSizeLimit` — each takes its one numeric argument and produces an `Instruction` with an **empty account-meta list** (compute-budget instructions take no accounts).
- **Symmetric `Binary` instance**: both `put` and `get`, verified by a round-trip property.

### Network.Solana.SplPrograms.Memo (new hierarchy)

- `memoProgramId :: SolanaPublicKey` = `MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr` (v2 only; v1 is legacy and out of scope).
- Instruction data is the raw UTF-8 bytes of the memo text — no discriminant, no length prefix.
- `buildMemo :: String -> [SolanaPublicKey] -> Instruction` — the pubkeys become readonly-signer account metas (list may be empty), matching SPL's `build_memo`.
- The `SplPrograms.*` hierarchy is the naming precedent for phase 3 (SPL Token, ATA).

### SystemInstruction `get` backfill

Phase 1 left `Binary SystemInstruction` encode-only (Generic-default `get` that does not parse what `put` writes). This phase adds a bincode-matching `get` for all 13 variants so the instance is symmetric before the pattern is copied further, verified by a round-trip property.

## Testing

Same golden-vector discipline as phase 1; `tools/fixture-gen` is extended and re-run once, fixtures committed.

**New fixtures** (fixed constants, mirrored between the Rust generator and Haskell tests):
- `compute_budget_instruction_data.json` — borsh bytes of the four live variants via the Rust SDK's own builder functions. Constants: heap 32768, unit limit 200000, unit price 1000, data-size limit 65536.
- `memo_instruction_data.json` — UTF-8 bytes of `"hello-memo"`. Built in Rust without new crate dependencies (program id from string + raw bytes).
- `transactions.json` gains `priority-transfer-transaction`: `[setComputeUnitLimit 200000, setComputeUnitPrice 1000, transfer 1000000000, memo "hello-memo" signed by payer]`, signed by the seed-1 payer with the 32×9 blockhash. First multi-program message golden (three program ids in one account list).

**Tests (TDD; tests land first and fail):**
- Golden: every ComputeBudgetInstruction variant; memo data bytes; the priority-transfer transaction byte-identical to Rust.
- Unit: compute-budget builders produce empty account lists; `buildMemo` produces readonly-signer metas in order.
- Properties: `decode . encode ≡ id` for `ComputeBudgetInstruction` and `SystemInstruction`.

## Housekeeping

- `exposed-modules` gains the two new modules.
- README: check off Compute Budget and Memo (Memo listed under a new SPL grouping consistent with the roadmap layout).
- CHANGELOG: bullets for the two clients and the `SystemInstruction` decoding support.
- No new Haskell dependencies; `-Wall`-clean; CI unchanged.

## Out of scope

- Memo v1 program; a general Borsh library; convenience wrappers that auto-prepend compute-budget instructions to transactions; any RPC-layer changes.

## Success criteria

1. Suite green (phase-1 33 tests + new ones) locally.
2. Every new instruction's bytes and the priority-transfer transaction byte-identical to Rust SDK vectors.
3. Round-trip properties hold for `ComputeBudgetInstruction` and `SystemInstruction`.
4. README/CHANGELOG updated accurately.
