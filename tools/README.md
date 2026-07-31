# tools/fixture-gen

Generates the golden test vectors in `test/fixtures/` from the official Rust
`solana-sdk`, so the Haskell SDK's serialization can be asserted byte-identical
to the reference implementation.

Fixtures are committed; regeneration is only needed when adding vectors:

    cd tools/fixture-gen
    cargo run

All fixed inputs (seeds, pubkey byte patterns, amounts) are mirrored as
constants in the Haskell test modules — if you change them here, change them
there too.
