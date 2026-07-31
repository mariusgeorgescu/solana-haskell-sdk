# Revision history for solana-haskell-sdk

## Unreleased

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

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
