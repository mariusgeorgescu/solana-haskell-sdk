# Stake + Vote Program Clients — Design

**Date:** 2026-08-01
**Status:** Approved (sub-project 4 of 6; autonomous-goal mode — scope decisions documented here)
**Goal:** Byte-verified clients for the Stake program and the Vote program's account-management surface.

## Context

Phases 1-3 delivered the hardened core, Compute Budget/Memo, and SPL Token/ATA/PDA (106 green tests). Stake and Vote are the remaining large native-program surfaces. Both use bincode (u32 LE discriminants) like the System Program, so the serialization machinery and helpers (`putBincodeString`, option tags) carry over.

Work on `development`; no pushes.

## Scope

### Network.Solana.NativePrograms.Stake (new)

- `stakeProgramId = "Stake11111111111111111111111111111111111111"`; `stakeConfigId = "StakeConfig11111111111111111111111111111111"`.
- Support types (bincode structs): `Authorized {staker, withdrawer :: SolanaPublicKey}`; `Lockup {lockupUnixTimestamp :: Int64, lockupEpoch :: Word64, lockupCustodian :: SolanaPublicKey}`; `StakeAuthorize = AuthorizeStaker | AuthorizeWithdrawer` (u32 LE 0/1); `LockupArgs` with three bincode `Option` fields (1-byte tag 0/1 + value).
- `data StakeInstruction` covering discriminants **0-7, 9, 10, 13**: Initialize(Authorized, Lockup), Authorize(Pubkey, StakeAuthorize), DelegateStake, Split(u64), Withdraw(u64), Deactivate, SetLockup(LockupArgs), Merge, InitializeChecked, AuthorizeChecked(StakeAuthorize), GetMinimumDelegation. **Deferred post-1.0:** 8/11 (seed-authority variants), 12 (SetLockupChecked), 14 (DeactivateDelinquent), 15 (Redelegate — deprecated upstream).
- Builders mirroring `solana_sdk::stake::instruction`: `initialize`, `authorize`, `delegateStake`, `split`, `withdraw` (with optional custodian signer), `deactivate`, `setLockup`, `merge`, `initializeChecked`, `authorizeChecked` — metas per the Rust source (clock/stake-history/stake-config sysvars where required). Symmetric `Binary` instance.

### Network.Solana.NativePrograms.Vote (new)

- `voteProgramId = "Vote111111111111111111111111111111111111111"`.
- Support types: `VoteInit {viNodePubkey, viAuthorizedVoter, viAuthorizedWithdrawer :: SolanaPublicKey, viCommission :: Word8}`; `VoteAuthorize = AuthorizeVoter | AuthorizeWithdrawer` (u32 LE 0/1).
- `data VoteInstruction` covering the **account-management surface**: InitializeAccount(VoteInit) = 0, Authorize(Pubkey, VoteAuthorize) = 1, Withdraw(u64) = 3, UpdateValidatorIdentity = 4, UpdateCommission(Word8) = 5, AuthorizeChecked(VoteAuthorize) = 7 (discriminants confirmed against the generated fixtures — the Rust crate is ground truth). **Deferred post-1.0:** the consensus-voting instructions (Vote=2, VoteSwitch=6, and the tower/state-update family 8-15) — they are produced by validators, not dApp clients.
- Builders mirroring `solana_vote_program::vote_instruction` for the covered set. Symmetric `Binary` instance.

## Testing

Same golden-vector discipline; `tools/fixture-gen` gains the `solana-vote-program` crate (generator-only).

- `stake_instruction_data.json` — data bytes for all 11 covered variants (SetLockup emitted twice: all-Some and all-None option fields).
- `vote_instruction_data.json` — data bytes for all 6 covered variants.
- `transactions.json` gains `stake-setup-transaction`: system createAccount(stake account) + stake initialize + delegateStake in one transaction signed by payer + the new stake account keypair (seed 32×10, reusing the phase-2 two-signer key) — exercises two signers + three programs + sysvar-heavy metas end-to-end.
- Tests: golden per variant; meta tests per builder (mirroring Rust source lists); round-trip properties for both instruction types; the transaction golden. TDD throughout.

## Housekeeping

- No new Haskell dependencies. README: Stake and Vote checked. CHANGELOG bullets.

## Out of scope

Stake account state parsing (StakeStateV2 deserialization), vote state parsing, the deferred instruction variants listed above, epoch-rewards queries.

## Success criteria

1. Suite green with all new tests.
2. Every covered Stake/Vote variant byte-identical to Rust vectors.
3. The stake-setup transaction byte-identical to Rust.
4. README/CHANGELOG accurate.
