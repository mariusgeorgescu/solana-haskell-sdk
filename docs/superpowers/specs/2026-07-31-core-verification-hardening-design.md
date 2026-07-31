# Core Verification & Hardening — Design

**Date:** 2026-07-31
**Status:** Approved (sub-project 1 of 6)
**Goal:** A stable, trustworthy 1.0.0.0 Hackage release, built in phases. This spec covers phase 1: prove the existing core is byte-compatible with Solana's wire format, fix what isn't, complete the System Program, and put CI in place.

## Context

The SDK builds cleanly (GHC 9.6.7, zero warnings) and has broad JSON-RPC coverage plus working core transaction machinery, but:

- The test suite is a stub (`test/Main.hs` prints "not yet implemented").
- Code reading found three likely wire-format bugs (detailed below).
- The System Program client implements only 4 of 13 instructions, though the README marks it complete.
- There is no CI; the CHANGELOG is a placeholder.

For a serialization-heavy SDK, unproven byte-compatibility is the central risk: every future program client inherits it. Hence foundation-first.

## Roadmap decomposition

Six sequential sub-projects, each with its own spec → plan → implementation cycle. This spec details only #1.

1. **Core verification & hardening** (this spec).
2. **Compute Budget + Memo clients** — small, high-value; proves the "add a program client" pattern on the hardened core.
3. **SPL Token + Associated Token Account clients** — requires adding PDA derivation (`createProgramAddress` / `findProgramAddress`) to the crypto core, which needs sha256 and an is-on-curve check (new `crypton` dependency).
4. **Stake + Vote clients** — the largest instruction surfaces.
5. **BPF Loader + Secp256k1 clients.**
6. **Release prep** — version 1.0.0.0, CHANGELOG, Haddock pass, GHC version matrix in CI, Hackage upload.

**Out of scope for the stable release:**

- **Metaplex** — needs Borsh serialization (a second serialization subsystem) and churns upstream; post-1.0, likely a separate package.
- **Versioned (v0) transactions and Address Lookup Tables** — legacy transactions fully cover every in-scope program; post-1.0.
- **Config program** — deprecated upstream; dropped from the roadmap entirely.

## Phase 1 design

### Test architecture

**Framework:** `tasty`, with `tasty-hunit` for golden/unit tests and `tasty-quickcheck` for properties. Test modules mirror `src`:

```
test/
  Main.hs                              -- tasty entry point
  Test/Core/Compact.hs
  Test/Core/Crypto.hs
  Test/Core/Message.hs
  Test/NativePrograms/SystemProgram.hs
  fixtures/                            -- checked-in golden vectors (JSON)
```

**Golden vectors (ground truth).** JSON fixtures pairing inputs with expected bytes (hex), produced by the official Rust `solana-sdk` via a small generator script kept in `tools/` (documented and re-runnable, but never executed in CI — fixtures are committed). Three levels:

- **Instruction-data:** the bincode bytes of every `SystemInstruction` variant (e.g. `Transfer { lamports }` → exact 12 bytes). Catches the seed length-prefix bug directly.
- **Message:** complete unsigned message bytes for fixed keypairs and a fixed blockhash. Catches account-ordering, header-count, and program-id-account bugs.
- **Transaction:** fully signed transaction bytes. Reproducible because Ed25519 signing is deterministic and test keypairs derive from fixed seeds (`createSolanaKeypairFromSeed`).

**Property tests (QuickCheck):**

- compact-u16: `decode . encode ≡ id` over all `Word16`; encoded length matches the canonical 1/2/3-byte rules; aliased/overlong encodings rejected.
- base58/base64 round-trips in `Core.Crypto`.
- `Binary` round-trips for public keys, signatures, message headers, and compiled messages.
- Message invariants: header counts consistent with account-list ordering (writable signers, readonly signers, writable non-signers, readonly non-signers); fee payer first; no duplicate keys.

**TDD ordering:** vectors and properties land first and must fail against the known bugs; fixes then turn them green. Any further mismatches the vectors expose are fixed in the same pass.

### Core fixes

All are internal changes acceptable pre-1.0:

1. **Seed serialization** (`SystemProgram.hs`, `CreateAccountWithSeed`): bincode encodes a Rust `String` as u64 little-endian byte length followed by UTF-8 bytes; the current `putStringUtf8` writes no length prefix. Fix with a shared `putBincodeString` helper, reused by the new `*WithSeed` instructions.
2. **Signature decoding** (`Core/Crypto.hs`, `Binary SolanaSignature`): `get` reads 32 bytes; Ed25519 signatures are 64.
3. **Program-id account handling** (`Core/Instruction.hs` `mkInstruction`, `Core/Message.hs`): `mkInstruction` currently appends the program id as an `AccountMeta` on every instruction, so it is passed to the program as a spurious account. Fix: `mkInstruction` stops appending it; message compilation instead adds each instruction's program id to the message account keys as readonly-unsigned (matching the Rust SDK), and compiled instructions index only their real accounts. Riskiest change — gated by the message-level golden vectors.
4. **Parse robustness** (`Core/Instruction.hs`, `FromJSON CompiledInstruction`): replace `fromJust` on base58 decoding with a proper parse failure. No other error-handling changes this phase.

### System Program completion

Add the nine missing instructions (indices 4–12), each with a builder function in the existing style and an instruction-level golden vector:

`AdvanceNonceAccount`, `WithdrawNonceAccount`, `InitializeNonceAccount`, `AuthorizeNonceAccount`, `Allocate`, `AllocateWithSeed`, `AssignWithSeed`, `TransferWithSeed`, `UpgradeNonceAccount`.

The nonce instructions reference the `RecentBlockhashes` and `Rent` sysvars already defined in `Network.Solana.Sysvar`.

### CI and housekeeping

- **GitHub Actions:** one workflow, one job — `cabal build all` + `cabal test` on GHC 9.6 with dependency caching. (GHC matrix deferred to release prep.)
- **README:** roadmap corrected — System Program marked accurately until complete, Config dropped, Compute Budget and Memo added, Metaplex moved to a post-1.0 section; broken Haddock badge link fixed or removed.
- **CHANGELOG:** real entry describing phase-1 changes.
- **Cabal:** add test-suite dependencies (`tasty`, `tasty-hunit`, `tasty-quickcheck`, plus `bytestring`/`aeson` for fixtures). No library dependency changes this phase.

## Success criteria

1. `cabal test` passes locally and in GitHub Actions.
2. Every `SystemInstruction` variant's encoding is byte-identical to the Rust SDK golden vector.
3. A signed transfer transaction is byte-identical to the Rust SDK golden vector.
4. All 13 System Program instructions are implemented with builders and tests.
5. README roadmap reflects reality.
