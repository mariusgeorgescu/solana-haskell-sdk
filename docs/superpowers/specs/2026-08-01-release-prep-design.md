# Release Prep (1.0.0.0) — Design

**Date:** 2026-08-01
**Status:** Approved (sub-project 6 of 6; autonomous-goal mode)
**Goal:** Make the SDK release-ready as version 1.0.0.0: accumulated polish closed, package metadata correct, docs buildable, CI matrixed, and the sdist proven to build and test from the tarball. The Hackage upload itself is the maintainer's act and out of scope.

## Scope

### 1. Accumulated polish (cross-phase carry-forwards)

- **Vote constructor naming**: rename to the house convention (bare names under qualified import): `VoteAuthorizeIx` → `Authorize`, `VoteWithdraw` → `Withdraw`, `VoteAuthorizeChecked` → `AuthorizeChecked`, `AuthorizeVoteWithdrawer` → `AuthorizeWithdrawer`. Mechanical rename in module + tests; suite green proves it. (Decision recorded: `bpfLoaderUpgradeableId` keeps its Rust-mirroring name — intentional deviation, Haddock-noted.)
- **Phase-4 test strengthening**: `initializeChecked` meta test with DISTINCT staker/withdrawer keys; Just-custodian meta tests for stake `withdraw` and `authorizeChecked`; a `voteProgramId` byte-pin test (base58-decodes to the canonical 32 bytes, asserted against a hex literal).
- **Stake `split` Haddock**: note the destination must be pre-allocated (200 bytes) and assigned to the stake program (Rust's composite `split` helper does this; ours is single-instruction).
- **Generator hygiene**: drop the unused `use solana_sdk::vote::{self};` import in tools/fixture-gen (no regeneration — fixtures untouched).

### 2. Package metadata

- Cabal: version `1.0.0.0`; `extra-source-files: test/fixtures/*.json` (sdist test-completeness — phase-1 review flagged this); refreshed synopsis/description mentioning the byte-verified program clients; `tested-with: GHC ==9.6.7`.
- README: replace the stale "no stable release / latest is v1.0.0 alpha" status text with a 1.0.0.0 statement; fill the empty Documentation section with a pointer to the Haddocks and the golden-vector methodology (tools/README.md).
- CHANGELOG: convert `## Unreleased` to `## 1.0.0.0 -- 2026-08-01` with a one-line release header.

### 3. Verification gates

- `cabal haddock` builds without errors (documentation coverage warnings acceptable, errors not).
- CI: matrix GHC 9.6 + 9.8 (9.8 as a second entry; if dependency bounds block it, keep 9.6-only and record why in the workflow file as a comment).
- **The sdist gate** (release-critical): `cabal sdist`, unpack the tarball to a temp dir, `cabal build && cabal test` from inside it — proving the released artifact is self-contained (fixtures packaged).

## Out of scope

Hackage upload (maintainer credentials); explicit-fee-payer API variant (post-1.0, recorded); Metaplex/v0/ALT (post-1.0 per phase-1 spec); renaming `bpfLoaderUpgradeableId`.

## Success criteria

1. Suite green (target ~179 tests after strengthening); `-Wall` clean.
2. Version 1.0.0.0 everywhere consistent (cabal, README, CHANGELOG).
3. `cabal haddock` succeeds; CI workflow updated.
4. Tarball-unpacked `cabal test` passes — the sdist is self-contained.
