# Release Prep (1.0.0.0) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Version 1.0.0.0, polish closed, docs buildable, CI matrixed, sdist proven self-contained.

**Architecture:** Three tasks: code/test polish, metadata, verification gates. No wire-format changes — the suite must stay green throughout (175 → ~179 with strengthened tests).

**Spec:** `docs/superpowers/specs/2026-08-01-release-prep-design.md`

## Global Constraints

- Branch `development`. Commit per task. Never push. `cabal build all` warning-clean (`-Wall`). No new dependencies. Fixtures NOT regenerated (the generator-import cleanup must not change any fixture file).

---

### Task 1: Polish — Vote rename, test strengthening, Haddock, generator hygiene

**Files:**
- Modify: `src/Network/Solana/NativePrograms/Vote.hs`, `test/Test/NativePrograms/Vote.hs` (rename)
- Modify: `src/Network/Solana/NativePrograms/Stake.hs` (split Haddock note only)
- Modify: `test/Test/NativePrograms/Stake.hs` (strengthened + new tests)
- Modify: `test/Test/NativePrograms/Vote.hs` (voteProgramId pin test)
- Modify: `tools/fixture-gen/src/main.rs` (drop unused import; NO regeneration)

- [ ] **Step 1: Vote constructor rename** — `VoteAuthorizeIx` → `Authorize`, `VoteWithdraw` → `Withdraw`, `VoteAuthorizeChecked` → `AuthorizeChecked`, `AuthorizeVoteWithdrawer` → `AuthorizeWithdrawer` in Vote.hs (type, Binary instance, builders) and the Vote test module (generator, goldens). Purely mechanical; suite must stay green (fixtures unchanged — the rename does not touch bytes).
- [ ] **Step 2: Stake test strengthening (TDD where new)** — in `test/Test/NativePrograms/Stake.hs`:
  1. `initializeChecked` meta test: introduce `withdrawerPk = unsafeSolanaPublicKeyRaw (replicate 32 28)` and use `Stake.Authorized {Stake.aStaker = payerPk, Stake.aWithdrawer = withdrawerPk}` so the expected metas become `[stake w, rent ro, payerPk ro non-signer, withdrawerPk s]` — now swap-detecting.
  2. New test "withdraw metas (custodian)": same as the no-custodian case plus `AccountMeta custodianPk True False` last, calling `withdraw ... (Just custodianPk)`.
  3. New test "authorizeChecked metas (custodian)": analogous.
- [ ] **Step 3: voteProgramId pin** — in `test/Test/NativePrograms/Vote.hs`: `testCase "voteProgramId decodes to canonical bytes" $ getSolanaPublicKeyRaw Vote.voteProgramId @?= fst (B16.decodeLenient ...)` — simplest correct form: compare `toBase58String (getSolanaPublicKeyRaw Vote.voteProgramId) @?= "Vote111111111111111111111111111111111111111"` AND `BS.length (getSolanaPublicKeyRaw Vote.voteProgramId) @?= 32` (pins round-trip through the IsString path; imports from Core.Crypto).
- [ ] **Step 4: Stake split Haddock** — add to `split`'s Haddock: "The destination account must already be allocated with 200 bytes and assigned to the stake program (compose with the System Program client's createAccount/allocate+assign); the Rust SDK's composite @split@ helper emits those instructions automatically, this builder does not."
- [ ] **Step 5: generator import cleanup** — delete `use solana_sdk::vote::{self};` from tools/fixture-gen/src/main.rs; run `cd tools/fixture-gen && cargo build && cd ../..` (build only — do NOT run; fixtures must be untouched: `git status test/fixtures/` clean).
- [ ] **Step 6:** Full suite green (175 + 3 new = 178... count: +2 custodian tests +1 pin = 178); `cabal build all` -Wall clean. Commit: `polish: vote constructor naming convention; stake test strengthening; split Haddock; generator hygiene`.

---

### Task 2: Package metadata

**Files:** `solana-haskell-sdk.cabal`, `README.md`, `CHANGELOG.md`

- [ ] **Step 1: cabal** — `version: 1.0.0.0`; add after `extra-doc-files`:

```cabal
extra-source-files:
  test/fixtures/*.json
```

Update `synopsis:` to `Solana SDK: transaction building, signing, program clients, and JSON-RPC.` and extend `description:` with one sentence: `All serialization is verified byte-for-byte against the official Rust SDK by golden-vector tests.` Keep `tested-with: GHC ==9.6.6` → change to `GHC ==9.6.7`.

- [ ] **Step 2: README** — replace the `### Current development status` paragraph ("There is currently **no stable release**...") with: `The current stable release is **v1.0.0.0**. Serialization is verified byte-for-byte against the official Rust SDK by golden-vector tests (see test/fixtures/ and tools/README.md).` Fill the empty `## Documentation` section with: `API documentation is generated with Haddock (\`cabal haddock\`). Serialization correctness methodology: every instruction, message, and transaction encoder is asserted byte-identical to vectors generated from the official Rust \`solana-sdk\` — see \`tools/README.md\`.`
- [ ] **Step 3: CHANGELOG** — change `## Unreleased` to `## 1.0.0.0 -- 2026-08-01` and insert directly under it: `First stable release: byte-verified clients for System, Compute Budget, Stake, Vote, BPF Loader (upgradeable), Secp256k1, SPL Memo, SPL Token, and Associated Token Account programs; PDA derivation; canonical Rust-identical message compilation.`
- [ ] **Step 4:** `cabal build all && cabal test` green (metadata only). Commit: `release: version 1.0.0.0 metadata, sdist fixtures, docs`.

---

### Task 3: Verification gates — Haddock, CI matrix, sdist

**Files:** `.github/workflows/ci.yml`; no other tracked files (sdist verification is ephemeral in /tmp-style scratch)

- [ ] **Step 1: Haddock gate** — `cabal haddock` must exit 0. If it errors on malformed Haddock markup, fix the offending comment minimally (document each fix).
- [ ] **Step 2: CI matrix** — in `.github/workflows/ci.yml`, convert the single job to a matrix `ghc: ["9.6", "9.8"]` (cabal 3.10; key caches by matrix ghc). FIRST verify locally whether the dependency bounds allow 9.8: `base ^>=4.18.0.0` PINS GHC 9.6 — widening base to `>=4.18 && <4.20` requires a full build under 9.8 which may not be installed. Decision rule: if `ghc-9.8` is not available locally (`ghc-9.8 --version` fails and `ghcup list` unavailable), keep the workflow at 9.6-only and add a YAML comment: `# GHC matrix deferred: base ^>=4.18 pins 9.6; widen bounds + verify before adding 9.8`. Do not ship an unverified matrix entry.
- [ ] **Step 3: THE SDIST GATE** — from the repo root:

```bash
cabal sdist
SDIST_DIR=$(mktemp -d)
tar -xzf dist-newstyle/sdist/solana-haskell-sdk-1.0.0.0.tar.gz -C "$SDIST_DIR"
cd "$SDIST_DIR/solana-haskell-sdk-1.0.0.0"
cabal build lib:solana-haskell-sdk solana-haskell-sdk-test && cabal test
```

The tarball test run MUST pass (this is the release-critical gate proving fixtures are packaged). If it fails on missing fixtures, the cabal `extra-source-files` from Task 2 is wrong — fix and repeat. Report the tarball test output. Clean up the temp dir.

- [ ] **Step 4:** Commit (`ci: GHC matrix decision; verified sdist self-containment` — only ci.yml changes tracked; if ci.yml unchanged because 9.6-only was already right, commit whatever changed or note no commit needed and fold the YAML comment in regardless).

---

## Success criteria (from the spec)

1. Suite green (178), -Wall clean — Task 1. 2. 1.0.0.0 consistent — Task 2. 3. Haddock + CI — Task 3. 4. Tarball test passes — Task 3.
