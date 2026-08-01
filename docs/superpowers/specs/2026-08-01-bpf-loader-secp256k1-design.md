# BPF Loader Upgradeable + Secp256k1 Clients — Design

**Date:** 2026-08-01
**Status:** Approved (sub-project 5 of 6; autonomous-goal mode — scope decisions documented here)
**Goal:** Byte-verified clients for the upgradeable BPF loader and the secp256k1 signature-verification precompile — the last two roadmap program clients.

## Context

Phases 1-4 delivered everything else (148 green tests). These two programs complete the README roadmap's native-program list. Work on `development`; no pushes.

## Scope

### Network.Solana.NativePrograms.BpfLoaderUpgradeable (new)

- `bpfLoaderUpgradeableId = "BPFLoaderUpgradeab1e11111111111111111111111"`.
- `data UpgradeableLoaderInstruction` covering ALL 8 discriminants (bincode u32 LE): InitializeBuffer=0, Write {offset :: Word32, bytes :: ByteString}=1 (bytes as bincode Vec<u8>: u64 LE length + raw bytes), DeployWithMaxDataLen {maxDataLen :: Word64}=2, Upgrade=3, SetAuthority=4, Close=5, ExtendProgram {additionalBytes :: Word32}=6, SetAuthorityChecked=7. Symmetric `Binary` (length guard on Vec<u8> like `getBincodeString`).
- `programDataAddress :: SolanaPublicKey -> Maybe SolanaPublicKey` — the PDA of [program id] under the loader (reuses `Core.Pda`).
- Single-instruction builders mirroring the Rust metas: `initializeBuffer` (buffer w, authority ro), `write` (buffer w, authority s), `deployWithMaxDataLen` (payer ws, programdata w, program w, buffer w, rent ro, clock ro, system ro, authority s), `upgrade` (programdata w, program w, buffer w, spill w, rent ro, clock ro, authority s), `setAuthority` (owned w, current authority s, new authority ro), `setAuthorityChecked` (owned w, current s, new s), `close` (account w, recipient w, authority s, optional program w), `extendProgram` (programdata w, program w, optional [system ro, payer ws]). Composite Rust helpers (`create_buffer`, `deploy_with_max_program_len` returning several instructions) are NOT mirrored — users compose with the System Program client; the Haddocks say so.

### Network.Solana.NativePrograms.Secp256k1 (new)

- `secp256k1ProgramId = "KeccakSecp256k11111111111111111111111111111"` (precompile; instructions carry NO accounts).
- `data SecpSignatureOffsets` — the 11-byte packed struct (u16 LE signatureOffset, u8 signatureInstructionIndex, u16 ethAddressOffset, u8 ethAddressInstructionIndex, u16 messageDataOffset, u16 messageDataSize, u8 messageInstructionIndex) with symmetric Binary.
- `newSecp256k1Instruction :: ByteString -> ByteString -> Word8 -> ByteString -> Instruction` (ethAddress 20B, signature 64B, recoveryId, message) — builds the single-signature instruction data exactly like Rust's `new_secp256k1_instruction`: count u8 = 1, offsets (DATA_START = 12: ethAddressOffset 12, signatureOffset 32, messageDataOffset 97), then eth address, signature, recovery id, message. **Scope decision:** the SDK does NOT sign — callers bring a precomputed secp256k1 signature and eth address (avoids a heavyweight secp256k1 dependency; signing belongs to the caller's key infrastructure). Input lengths are validated (Either-returning smart constructor or documented error — decided in plan: validate and `error` with clear messages, matching the module's precompile semantics, documented in Haddock).

## Testing

- Fixture generation: `tools/fixture-gen` gains a `libsecp256k1` dev-dependency (generator-only) to produce a REAL signed vector via Rust's `new_secp256k1_instruction` with a fixed secret key (32 bytes of 42) over message `"hello-secp"`. Emitted: the full instruction data plus the components (eth-address, signature, recovery-id) as separate entries, so the Haskell test reconstructs via our builder and asserts byte-identity — proving the offsets/layout logic against Rust's own signer.
- `loader_instruction_data.json`: all 8 loader variants (Write with offset 128 and bytes = 64 bytes of 0xAB; DeployWithMaxDataLen 1048576; ExtendProgram 4096).
- Loader tests: goldens, meta tests per builder, round-trip property, unknown-discriminant; `programDataAddress` golden (fixture from Rust `find_program_address` for a fixed program id 32×23 under the loader).
- Secp tests: offsets-struct golden (decoded from the full vector), full-instruction reconstruction golden, length-validation cases.

## Housekeeping

No new Haskell dependencies. README: BPF Loader and Secp256k1 checked (with scope qualifiers). CHANGELOG bullets.

## Out of scope

Legacy (non-upgradeable) BPF loaders; loader-v4; composite deploy/create-buffer helpers; secp256k1 signing/recovery; secp256r1 precompile; ed25519 precompile.

## Success criteria

1. Suite green with all new tests.
2. All 8 loader variants + the secp instruction byte-identical to Rust vectors.
3. `programDataAddress` reproduces Rust for the fixture input.
4. README/CHANGELOG accurate.
