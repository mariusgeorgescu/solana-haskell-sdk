{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Secp256k1 native precompile program. The precompile
-- verifies secp256k1 (ECDSA/@ecrecover@) signatures included directly in an
-- instruction's data against a message and Ethereum address, also included
-- in that data; it does not compute a signature itself. Signing (producing
-- the signature and recovery id from a private key) is out of scope for this
-- SDK — callers must supply an already-computed signature.
module Network.Solana.NativePrograms.Secp256k1 where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.Core.Instruction

-- | Secp256k1 native precompile program address.
secp256k1ProgramId :: SolanaPublicKey
secp256k1ProgramId = "KeccakSecp256k11111111111111111111111111111"

-- | Offsets (within the transaction's instructions) of the signature,
-- Ethereum address, and message data used to verify a single secp256k1
-- signature. An instruction index of @0@ refers to this instruction itself
-- (the value the Rust builder uses; unlike the ed25519 precompile, there is
-- no dedicated "current instruction" sentinel for secp256k1). Serialized as
-- an 11-byte little-endian packed struct, matching the Rust SDK layout.
data SecpSignatureOffsets = SecpSignatureOffsets
  { -- | Offset to the [signature, recovery_id] bytes (64 + 1 bytes).
    ssoSignatureOffset :: Word16,
    -- | Instruction index to find the signature.
    ssoSignatureInstructionIndex :: Word8,
    -- | Offset to the Ethereum address (20 bytes).
    ssoEthAddressOffset :: Word16,
    -- | Instruction index to find the Ethereum address.
    ssoEthAddressInstructionIndex :: Word8,
    -- | Offset to the start of the message data.
    ssoMessageDataOffset :: Word16,
    -- | Size of the message data, in bytes.
    ssoMessageDataSize :: Word16,
    -- | Instruction index to find the message data.
    ssoMessageInstructionIndex :: Word8
  }
  deriving (Eq, Show)

instance Binary SecpSignatureOffsets where
  put :: SecpSignatureOffsets -> Put
  put (SecpSignatureOffsets sigOffset sigIdx ethOffset ethIdx msgOffset msgSize msgIdx) = do
    putWord16le sigOffset
    putWord8 sigIdx
    putWord16le ethOffset
    putWord8 ethIdx
    putWord16le msgOffset
    putWord16le msgSize
    putWord8 msgIdx

  get :: Get SecpSignatureOffsets
  get =
    SecpSignatureOffsets
      <$> getWord16le
      <*> getWord8
      <*> getWord16le
      <*> getWord8
      <*> getWord16le
      <*> getWord16le
      <*> getWord8

-- | Secp256k1 instruction data: a pre-built byte string. The 'Binary'
-- instance mirrors 'Network.Solana.SplPrograms.Memo.MemoData': 'put' emits
-- the bytes verbatim (no discriminant, no length prefix) and 'get' consumes
-- all remaining input, so that 'mkInstruction' reproduces exactly the bytes
-- built by 'newSecp256k1Instruction'.
newtype Secp256k1InstructionData = Secp256k1InstructionData BS.ByteString
  deriving (Eq, Show)

instance Binary Secp256k1InstructionData where
  put :: Secp256k1InstructionData -> Put
  put (Secp256k1InstructionData bs) = putByteString bs
  get :: Get Secp256k1InstructionData
  get = Secp256k1InstructionData . BL.toStrict <$> getRemainingLazyByteString

-- | Creates a Secp256k1 signature-verification instruction that checks a
-- single, precomputed ECDSA signature: this SDK's counterpart of the Rust
-- SDK's @new_secp256k1_instruction_with_signature@. Receives the 20-byte
-- Ethereum address the signature is expected to recover to, the 64-byte
-- compact ECDSA signature, the recovery id, and the (Keccak-256 hashed, per
-- the precompile's expectations) message. The signature, address, and
-- message are all embedded in this single instruction's data (instruction
-- index @0@), so the precompile takes no accounts.
--
-- Throws via 'error' if @ethAddress@ is not exactly 20 bytes or @signature@
-- is not exactly 64 bytes, mirroring the assertions in the Rust builder.
newSecp256k1Instruction :: BS.ByteString -> BS.ByteString -> Word8 -> BS.ByteString -> Instruction
newSecp256k1Instruction ethAddress signature recoveryId message
  | BS.length ethAddress /= 20 =
      error ("newSecp256k1Instruction: ethAddress must be 20 bytes, got " <> show (BS.length ethAddress))
  | BS.length signature /= 64 =
      error ("newSecp256k1Instruction: signature must be 64 bytes, got " <> show (BS.length signature))
  | otherwise =
      mkInstruction secp256k1ProgramId [] (Secp256k1InstructionData (BL.toStrict (runPut body)))
  where
    offsets =
      SecpSignatureOffsets
        { ssoSignatureOffset = 32,
          ssoSignatureInstructionIndex = 0,
          ssoEthAddressOffset = 12,
          ssoEthAddressInstructionIndex = 0,
          ssoMessageDataOffset = 97,
          ssoMessageDataSize = fromIntegral (BS.length message),
          ssoMessageInstructionIndex = 0
        }
    body = do
      putWord8 1 -- count: a single signature to verify
      put offsets
      putByteString ethAddress
      putByteString signature
      putWord8 recoveryId
      putByteString message
