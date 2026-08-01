# Revision history for solana-haskell-sdk

## Unreleased

* Added Address Lookup Table program client and versioned (v0) transaction support: compile messages against lookup tables and sign VersionedTransactions (encode-only; account-state parsing out of scope).

## 1.0.0.0 -- 2026-08-01

First stable release: byte-verified clients for System, Compute Budget, Stake, Vote, BPF Loader (upgradeable), Secp256k1, SPL Memo, SPL Token, and Associated Token Account programs; PDA derivation; canonical Rust-identical message compilation.

* Changed: `sendTransaction`'s default configuration no longer skips preflight
  simulation and no longer caps retries — submissions now follow the node's
  defaults (preflight runs; the node rebroadcasts until blockhash expiry).
  Transactions failing simulation return an RPC error instead of a signature.
* Changed: account-fetching RPC methods now request `base64` encoding
  explicitly for reliable binary account data.
* Hardened all RPC-facing JSON parsers to be total: malformed or unexpected
  node responses (failed transactions, v0 transactions, pruned/unknown slots)
  no longer crash the client.
* Fixed: `mkPrivateKeyFromString` rejected valid 64-byte private keys; now
  parses them correctly.
* Fixed: `BlockHash` and `CompactArray` `Binary` decoding were asymmetric
  with their `put` (falling back to an incompatible/length-prefixed
  decoder); both now round-trip correctly.
* Fixed documentation typos referring to the `RPCResponse` type (the
  exported name never changed) and renamed RPC fields for consistency
  (`Ammount*` -> `Amount*`); changed transaction/account error fields to
  structured `Maybe Value` instead of partial `Maybe String`.
* Renamed `ClusterNodes.sharedVersion` to `shredVersion` (typo fix; matches
  the RPC field).
* Added `confirmTransaction` to `SolanaWeb3`: polls for `confirmed`/
  `finalized` status instead of a fixed `wait`.
* Haddock coverage completed for the Core, RPC/HTTP, and SolanaWeb3 modules
  touched by the quality pass (module headers and per-export documentation);
  `cabal haddock` exits 0 with a handful of internal native/SPL-program
  helpers still undocumented.
* Added upgradeable BPF loader client (buffer/write/deploy/upgrade/authority/
  close/extend) with program-data address derivation.
* Added secp256k1 precompile client: signature-verification instruction
  construction from a precomputed signature (signing is out of scope).
* Added Stake program client (initialize, authorize, delegate, split, withdraw,
  deactivate, lockup, merge, checked variants) — golden-tested against the Rust SDK.
* Added Vote program client (account management: initialize, authorize, withdraw,
  identity/commission updates); consensus voting instructions are out of scope.
* Added SPL Token program client (instructions 0-20: mint/account initialization,
  transfers, approvals, minting, burning, freezing, multisig support) —
  golden-tested against the `spl-token` Rust crate.
* Added Associated Token Account program client with address derivation.
* Added Program Derived Address (PDA) support: `createProgramAddress` /
  `findProgramAddress` with ed25519 on-curve rejection (new `crypton` and `memory` dependencies).
* Added Compute Budget program client: request heap frame, set compute unit
  limit, set compute unit price (priority fees), set loaded accounts data size
  limit — golden-tested against the Rust SDK.
* Added SPL Memo (v2) program client.
* Added binary decoding for `SystemInstruction` and `ComputeBudgetInstruction`
  (symmetric `Binary` instances, round-trip tested).
* Fixed: account keys within each privilege section are now ordered
  canonically (sorted by pubkey, fee payer first) to match the Rust SDK, so
  multi-program messages serialize byte-identically. For multi-signer
  transactions, signing keys must be passed in the canonical account order
  (fee payer first).
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
* Fixed: account privilege deduplication in message building used the wrong
  buckets to decide writability, producing duplicate keys or wrongly-writable
  accounts when the same account appeared in several instructions with mixed
  privileges.
* Fixed: compact-u16 decoding now rejects aliased (non-canonical) encodings.
* Fixed: `CompiledInstruction` JSON parsing fails cleanly on invalid base58.
* Added the 9 remaining System Program instructions: nonce account management
  (advance, withdraw, initialize, authorize, upgrade) and
  allocate/assign/transfer with seed.
* Added CI (GitHub Actions).

## 0.1.0.0 -- 2024 (unreleased alpha)

* Initial alpha: JSON-RPC API client, key management, System Program transfers.
