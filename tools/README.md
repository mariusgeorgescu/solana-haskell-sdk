# Test-vector generators

Two generators feed `test/fixtures/`. Both write committed fixtures that the
test suite replays offline, so CI never needs Rust or a validator.

## tools/fixture-gen

Generates the golden test vectors in `test/fixtures/` from the official Rust
crates (solana-sdk, spl-token, spl-associated-token-account, mpl-token-metadata), so the Haskell SDK's serialization can be asserted byte-identical
to the reference implementation.

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
transfer, an SPL mint with a token account holding 42 tokens at 6 decimals —
then calls every method the SDK exposes a parser for:

    solana-test-validator --reset          # in another terminal
    python3 tools/rpc-record/record.py

Requires the `solana` and `spl-token` CLIs on PATH. Set `SOLANA_RPC_URL` to
record against a different endpoint.

Re-recording rewrites the whole file, and pubkeys, slots and blockhashes all
change from run to run, so the diff is never small. Inspect what changed in
kind rather than in detail, and keep assertions in `Test.RPC.Parsers` tied to
values the script controls (the minted amount, the number of accounts) rather
than to recorded identifiers.

Note that everything is queried at the node's default `finalized` commitment.
State created during setup is finalized before recording — an earlier version
of the script skipped that wait and captured a token supply of `0`, which the
parser tests caught.
