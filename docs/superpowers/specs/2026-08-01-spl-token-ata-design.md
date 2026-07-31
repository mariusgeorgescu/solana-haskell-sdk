# SPL Token + Associated Token Account Clients — Design

**Date:** 2026-08-01
**Status:** Approved (sub-project 3 of 6; autonomous-goal mode — scope decisions documented here in lieu of interactive review)
**Goal:** Byte-verified clients for the SPL Token program and the Associated Token Account (ATA) program, plus the Program Derived Address (PDA) machinery they require.

## Context

Phases 1-2 delivered a hardened core (canonical Rust-identical message compilation, golden-vector discipline, 48 green tests). SPL Token is the highest-value program for dApp developers; ATA depends on PDA derivation, which enters the crypto core here.

Work on `development`; no pushes.

## Scope

### PDA derivation (Network.Solana.Core.Pda — new module)

- `createProgramAddress :: [ByteString] -> SolanaPublicKey -> Either PdaError SolanaPublicKey` — SHA-256 of `concat seeds <> programId <> "ProgramDerivedAddress"`; fails if any seed exceeds 32 bytes, more than 16 seeds, or the resulting hash IS a valid ed25519 curve point (on-curve).
- `findProgramAddress :: [ByteString] -> SolanaPublicKey -> Maybe (SolanaPublicKey, Word8)` — tries bump 255 downto 0 appended as a 1-byte seed; returns the first off-curve result and its bump.
- On-curve check and SHA-256 come from the **`crypton`** library (`Crypto.Hash` for SHA256; `Crypto.ECC.Edwards25519` point decoding as the curve-membership validator). **This is the one new library dependency of the stable release**, anticipated by the roadmap.
- Golden-verified: fixtures from Rust `Pubkey::find_program_address` / `get_associated_token_address`, chosen so the bump is < 255 (proves the search and on-curve rejection actually run).

### Network.Solana.SplPrograms.Token (new)

- `tokenProgramId = "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"`.
- `data TokenInstruction` covering instructions **0-20** of the SPL Token program: InitializeMint, InitializeAccount, InitializeMultisig, Transfer, Approve, Revoke, SetAuthority, MintTo, Burn, CloseAccount, FreezeAccount, ThawAccount, TransferChecked, ApproveChecked, MintToChecked, BurnChecked, InitializeAccount2, SyncNative, InitializeAccount3, InitializeMultisig2, InitializeMint2. Instructions 21-24 (GetAccountDataSize, InitializeImmutableOwner, AmountToUiAmount, UiAmountToAmount) are deferred post-1.0 (niche/off-chain helpers).
- Wire format is SPL Token's hand-rolled `Pack` (NOT bincode): u8 discriminant; u64 LE amounts; u8 decimals; raw 32-byte pubkeys; optional pubkeys (`COption<Pubkey>`) as a 1-byte tag (0/1) + 32 bytes when present; `AuthorityType` as u8 (0-3). Ground truth is the `spl-token` Rust crate via fixture-gen — golden vectors for every variant.
- Builders mirror the Rust `spl_token::instruction::*` helpers, including multisig-signer list support (`Maybe`-less: owner + `[SolanaPublicKey]` signer list, empty for single-owner), with account metas matching Rust exactly. Symmetric `Binary` instance (get parses what put writes, fails on unknown discriminant/tag).

### Network.Solana.SplPrograms.AssociatedTokenAccount (new)

- `associatedTokenProgramId = "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL"`.
- `getAssociatedTokenAddress :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey` (wallet → mint → ATA) — PDA of [wallet, tokenProgramId, mint] under the ATA program.
- Builders `createAssociatedTokenAccount`, `createAssociatedTokenAccountIdempotent` (funder, wallet, mint) — data is the 1-byte borsh discriminant (0 / 1); metas per the Rust crate (funder ws, ata w, wallet ro, mint ro, system program ro, token program ro). RecoverNested deferred post-1.0.

## Testing

Same discipline: `tools/fixture-gen` gains `spl-token` and `spl-associated-token-account` crate dependencies (generator-only; CI never runs cargo). New fixtures:

- `token_instruction_data.json` — data bytes for all 21 variants with fixed constants (amount 1000000, decimals 6, authority-type vectors for all four types, COption both None and Some cases).
- `pda.json` — `find_program_address` result + bump for fixed seeds; `get_associated_token_address` for fixed wallet/mint (generator asserts bump < 255).
- `transactions.json` gains `ata-transfer-transaction`: createAssociatedTokenAccountIdempotent + transferChecked in one signed transaction (exercises PDA + multi-program + canonical ordering end-to-end).

Tests: golden per variant; account-meta unit tests per builder; PDA golden + property (createProgramAddress rejects >32-byte seeds); round-trip property for `TokenInstruction`; the transaction golden. TDD throughout.

## Housekeeping

- `crypton` added to library `build-depends` (the sole new dependency).
- README: SPL Token + ATA checked; CHANGELOG bullets.
- Phase-2 carry-forwards folded in: a unit test for `getBincodeString`'s length guard.

## Out of scope

Token-2022 (extensions), instructions 21-24, RecoverNested, ui-amount helpers, explicit-fee-payer API rework (revisit in phase 6 if time allows).

## Success criteria

1. Suite green with all new tests.
2. Every TokenInstruction variant + ATA instruction byte-identical to Rust vectors.
3. `getAssociatedTokenAddress` reproduces Rust for the fixture inputs (bump < 255).
4. The ata-transfer transaction byte-identical to Rust.
5. README/CHANGELOG accurate.
