# Test-vector generators

Two generators feed `test/fixtures/`. Both write committed fixtures that the
test suite replays offline, so CI never needs Rust or a validator.

## tools/fixture-gen

Generates the golden test vectors in `test/fixtures/` from the official Rust
crates (solana-sdk, spl-token, spl-associated-token-account, mpl-token-metadata), so the Haskell SDK's serialization can be asserted byte-identical
to the reference implementation. `Cargo.lock` pins solana-sdk 1.18.26,
spl-token 4.0.3, spl-associated-token-account 3.0.4 and mpl-token-metadata
4.1.2.

The `{name, hex}` vectors pin instruction data; account-meta lists are
Rust-pinned only where an `accounts` entry exists (`create-v3-full-accounts`,
`master-edition-some-accounts`) or a compiled message/transaction vector in
`messages.json`/`transactions.json` covers the builder. Other builders' metas
are asserted against hand-written expectations in the Haskell tests.

Fixtures are committed; regeneration is only needed when adding vectors:

    cd tools/fixture-gen
    cargo run

All fixed inputs (seeds, pubkey byte patterns, amounts) are mirrored as
constants in the Haskell test modules — if you change them here, change them
there too.

## tools/rpc-record

Records real JSON-RPC responses into `test/fixtures/rpc_responses.json`, which
`Test.RPC.Parsers` replays to assert the `FromJSON` instances against JSON a
node actually sends. The response parsers are the one layer byte-level vectors
cannot reach, since their input is a node's JSON rather than a Rust struct's
serialization.

The script drives a local validator into a known state — a funded account, a
transfer, an SPL mint with a token account holding 42 tokens at 6 decimals and
a delegate — then calls every method the SDK binds except `requestAirdrop` and
`sendTransaction` (they mutate the ledger) and `simulateTransaction` (it needs
a signed transaction as input and returns an untyped `Object`):

    solana-test-validator --reset --slots-per-epoch 32   # in another terminal
    python3 tools/rpc-record/record.py

Requires the `solana` and `spl-token` CLIs on PATH. Set `SOLANA_RPC_URL` to
record against a different endpoint.

`--slots-per-epoch 32` is what makes `getInflationReward` recordable: on the
default 432,000-slot epoch the node cannot answer it until the first epoch
ends, so the script refuses to run on such a validator, waits for epoch 2, and
queries the bootstrap stake account's reward for the previous epoch. Any
method the node rejects aborts the run rather than being skipped, so the
fixture set cannot drift from this description silently.

Re-recording rewrites the whole file, and pubkeys, slots and blockhashes all
change from run to run, so the diff is never small. Inspect what changed in
kind rather than in detail, and keep assertions in `Test.RPC.Parsers` tied to
values the script controls (the minted amount, the number of accounts) rather
than to recorded identifiers.

Note that everything is queried at the node's default `finalized` commitment,
except `isBlockhashValid` and `getFeeForMessage`, which are queried at
`processed` so a just-fetched blockhash is known to the node.
State created during setup is finalized before recording — an earlier version
of the script skipped that wait and captured a token supply of `0`, which the
parser tests caught.
