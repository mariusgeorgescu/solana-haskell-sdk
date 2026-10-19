{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Network.Solana.Core.MessageV1
-- Description : v1 message compilation (SIMD-0385) and v1 transaction signing.
--
-- The v1 transaction format carries its resource requests in a header
-- configuration mask instead of @ComputeBudget@ instructions, lists every
-- address inline (it has no address lookup tables), and places the
-- signatures /after/ the message rather than before it. See
-- "Network.Solana.Core.VersionedMessage" for v0, which is the format to use
-- when lookup tables are needed.
module Network.Solana.Core.MessageV1
  ( TransactionConfig (..),
    emptyTransactionConfig,
    compileV1Message,
    compileV1MessageWithPayer,
    newV1TransactionIntent,
    newV1TransactionIntentWithPayer,
  )
where

import Data.Binary (Word16, Word32, Word64, Word8, put)
import Data.Binary.Put
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import GHC.Generics (Generic)
import Network.Solana.Core.Block (BlockHash)
import Network.Solana.Core.Compact (unCompact)
import Network.Solana.Core.Crypto
  ( SolanaPrivateKey,
    SolanaPublicKey,
    dsign,
    getSolanaSignatureRaw,
    toBase64String,
  )
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message

------------------------------------------------------------------------------------------------

-- * TransactionConfig

------------------------------------------------------------------------------------------------

-- | The resource requests a v1 message carries in place of the
-- @ComputeBudget@ instructions earlier formats use. Each field present
-- sets its bit in the message's configuration mask and appends its value;
-- each field absent leaves the network's default in force, and those
-- defaults are not the ones @ComputeBudget@ callers are used to:
-- a missing compute-unit limit means 0 (not a per-instruction allowance),
-- and a missing loaded-accounts size limit means 0 (not 64 MiB).
--
-- @ComputeBudget@ instructions placed in a v1 transaction are ignored for
-- configuration purposes -- they still execute, as successful no-ops that
-- consume compute units -- so resource requests must be made here.
data TransactionConfig = TransactionConfig
  { -- | Total priority fee, in lamports.
    tcPriorityFee :: Maybe Word64,
    -- | Maximum compute units the transaction may consume.
    tcComputeUnitLimit :: Maybe Word32,
    -- | Maximum bytes of account data the transaction may load.
    tcLoadedAccountsDataSizeLimit :: Maybe Word32,
    -- | Heap size in bytes. Must be a multiple of 1024 and within
    -- [32768, 262144]; 'compileV1Message' rejects anything else.
    tcHeapSize :: Maybe Word32
  }
  deriving (Show, Eq, Generic)

-- | A configuration requesting nothing, leaving every network default in
-- force (see 'TransactionConfig' for what those defaults are).
emptyTransactionConfig :: TransactionConfig
emptyTransactionConfig =
  TransactionConfig
    { tcPriorityFee = Nothing,
      tcComputeUnitLimit = Nothing,
      tcLoadedAccountsDataSizeLimit = Nothing,
      tcHeapSize = Nothing
    }

------------------------------------------------------------------------------------------------

-- * Compile v1 message

------------------------------------------------------------------------------------------------

-- | Compile a v1 message: assemble the account keys exactly as the legacy
-- and v0 builders do (privilege union, then canonical ordering, via
-- 'mkNewMessage'), then serialize them in the v1 layout behind the @129@
-- version byte.
--
-- Fails with a 'CompileException' when the result would not be a valid v1
-- message: more than 64 addresses or 64 instructions, more than 12
-- required signatures, an instruction with more than 255 accounts or more
-- than 65535 data bytes, or a heap request that is not a multiple of 1024
-- within [32768, 262144].
compileV1Message :: BlockHash -> [Instruction] -> TransactionConfig -> Either CompileException BS.ByteString
compileV1Message bh instructions = compileV1FromLegacy (mkNewMessage bh instructions)

-- | Like 'compileV1Message', but with an explicitly named fee payer, seeded
-- as a writable signer and pinned to account 0 via 'mkNewMessageWithPayer'.
-- Required when no instruction marks a signer writable (a memo-only or
-- authority-only instruction list) and for sponsored fees.
compileV1MessageWithPayer ::
  SolanaPublicKey -> BlockHash -> [Instruction] -> TransactionConfig -> Either CompileException BS.ByteString
compileV1MessageWithPayer payer bh instructions =
  compileV1FromLegacy (mkNewMessageWithPayer payer bh instructions)

-- | Serialize an assembled legacy message in the v1 layout (shared body of
-- 'compileV1Message' and 'compileV1MessageWithPayer').
compileV1FromLegacy :: Message -> TransactionConfig -> Either CompileException BS.ByteString
compileV1FromLegacy legacy config = do
  compiled <- mapM (compileInstruction keys) (mInstructions legacy)
  checkLimits legacy compiled config
  let bytes = BL.toStrict . runPut $ do
        putWord8 v1Prefix
        put (mHeader legacy)
        putWord32le (configMask config)
        put (mRecentBlockhash legacy)
        putWord8 (fromIntegral (length compiled))
        putWord8 (fromIntegral (length keys))
        mapM_ put keys
        putConfigValues config
        mapM_ putInstructionHeader compiled
        mapM_ putInstructionPayload compiled
  checkTransactionSize legacy bytes
  return bytes
  where
    keys = mAccountKeys legacy

-- | Sign a v1-compiled message with each signer's private key, and return
-- the Base64-encoded signed transaction. A v1 transaction puts the message
-- first and the signatures last, as a bare sequence of 64-byte signatures
-- with no length prefix -- the reverse of the legacy and v0 envelopes.
--
-- The private keys passed here must be ordered to match the resulting
-- account order (fee payer first), otherwise the produced signatures will
-- not correspond to the right account keys. If you would rather name the
-- fee payer explicitly and not worry about signer order, use
-- 'newV1TransactionIntentWithPayer'.
newV1TransactionIntent ::
  [SolanaPrivateKey] -> [Instruction] -> TransactionConfig -> BlockHash -> Either CompileException String
newV1TransactionIntent signers instructions config bh = do
  msg <- compileV1Message bh instructions config
  return $ toBase64String (msg <> signaturesOver msg signers)

-- | Like 'newV1TransactionIntent', but with an explicitly named fee payer
-- (see 'compileV1MessageWithPayer'); the given private keys may be listed
-- in any order -- they are matched against the message's required signers
-- and placed in message order automatically, with the same 'Left' cases as
-- 'Network.Solana.Core.Message.newTransactionIntentWithPayer'.
newV1TransactionIntentWithPayer ::
  SolanaPublicKey ->
  [SolanaPrivateKey] ->
  [Instruction] ->
  TransactionConfig ->
  BlockHash ->
  Either CompileException String
newV1TransactionIntentWithPayer payer signingKeys instructions config bh = do
  let legacy = mkNewMessageWithPayer payer bh instructions
  msg <- compileV1FromLegacy legacy config
  orderedKeys <- orderSigningKeys legacy signingKeys
  return $ toBase64String (msg <> signaturesOver msg orderedKeys)

------------------------------------------------------------------------------------------------

-- * Internal helpers

------------------------------------------------------------------------------------------------

-- | The trailing signature array: each signer's 64 raw signature bytes over
-- the whole serialized message, concatenated without a length prefix.
signaturesOver :: BS.ByteString -> [SolanaPrivateKey] -> BS.ByteString
signaturesOver msg signers = mconcat (getSolanaSignatureRaw . flip dsign msg <$> signers)

-- | The version byte a v1 transaction opens with.
v1Prefix :: Word8
v1Prefix = 129

-- | The configuration mask: one bit per requested field, except the
-- priority fee, which occupies two (both must be set, and its value is
-- eight bytes wide rather than four).
configMask :: TransactionConfig -> Word32
configMask TransactionConfig {..} =
  bitFor tcPriorityFee 0b11
    .|. bitFor tcComputeUnitLimit 0b100
    .|. bitFor tcLoadedAccountsDataSizeLimit 0b1000
    .|. bitFor tcHeapSize 0b10000
  where
    bitFor field bits = maybe 0 (const bits) field

-- | The requested values, in field order, each present only when its mask
-- bit is set.
putConfigValues :: TransactionConfig -> Put
putConfigValues TransactionConfig {..} = do
  mapM_ putWord64le tcPriorityFee
  mapM_ putWord32le tcComputeUnitLimit
  mapM_ putWord32le tcLoadedAccountsDataSizeLimit
  mapM_ putWord32le tcHeapSize

-- | One instruction's fixed-width header: program id index, account count,
-- and data length. All headers precede all payloads on the wire.
putInstructionHeader :: CompiledInstruction -> Put
putInstructionHeader CompiledInstruction {..} = do
  putWord8 ciProgramIdIndex
  putWord8 (fromIntegral (length (unCompact ciAccounts)))
  putWord16le (fromIntegral (length (unCompact ciData)))

-- | One instruction's variable-width payload: its account indices followed
-- by its data bytes, neither length-prefixed (the header carries both
-- lengths).
putInstructionPayload :: CompiledInstruction -> Put
putInstructionPayload CompiledInstruction {..} = do
  mapM_ putWord8 (unCompact ciAccounts)
  mapM_ putWord8 (unCompact ciData)

-- | The v1 sanitization limits this builder can check before serializing.
checkLimits :: Message -> [CompiledInstruction] -> TransactionConfig -> Either CompileException ()
checkLimits legacy compiled TransactionConfig {..} = do
  limit "addresses" maxAddresses (length (mAccountKeys legacy))
  limit "instructions" maxInstructions (length compiled)
  limit "required signatures" maxSignatures (fromIntegral (numRequiredSignatures (mHeader legacy)))
  mapM_ checkInstruction compiled
  mapM_ checkHeapSize tcHeapSize
  where
    limit what cap actual
      | actual <= cap = Right ()
      | otherwise =
          Left (MissingIndex ("v1 message has " <> show actual <> " " <> what <> ", more than the " <> show cap <> " allowed"))
    checkInstruction CompiledInstruction {..} = do
      limit "accounts in one instruction" maxInstructionAccounts (length (unCompact ciAccounts))
      limit "data bytes in one instruction" maxInstructionData (length (unCompact ciData))
    checkHeapSize n
      | n `mod` 1024 /= 0 =
          Left (MissingIndex ("v1 heap size " <> show n <> " is not a multiple of 1024"))
      | n < minHeapSize || n > maxHeapSize =
          Left
            ( MissingIndex
                ("v1 heap size " <> show n <> " is outside [" <> show minHeapSize <> ", " <> show maxHeapSize <> "]")
            )
      | otherwise = Right ()

-- | Reject a message whose transaction -- the message plus one 64-byte
-- signature per required signer -- would exceed the v1 size cap. Checked
-- on the message, since the header fixes the signature count regardless of
-- how many keys are later passed to sign it.
checkTransactionSize :: Message -> BS.ByteString -> Either CompileException ()
checkTransactionSize legacy bytes
  | total <= maxTransactionSize = Right ()
  | otherwise =
      Left
        ( MissingIndex
            ( "v1 transaction is "
                <> show total
                <> " bytes, more than the "
                <> show maxTransactionSize
                <> " allowed"
            )
        )
  where
    signatures = signatureSize * fromIntegral (numRequiredSignatures (mHeader legacy))
    total = BS.length bytes + signatures

-- | Addresses are counted by a single byte and capped at 64 by SIMD-0385.
maxAddresses :: Int
maxAddresses = 64

-- | Instructions are counted by a single byte and capped at 64.
maxInstructions :: Int
maxInstructions = 64

-- | A v1 transaction may carry at most 12 signatures.
maxSignatures :: Int
maxSignatures = 12

-- | An instruction's account count is a single byte.
maxInstructionAccounts :: Int
maxInstructionAccounts = fromIntegral (maxBound :: Word8)

-- | An instruction's data length is a 'Word16'.
maxInstructionData :: Int
maxInstructionData = fromIntegral (maxBound :: Word16)

-- | A v1 transaction may be at most 4096 bytes on the wire (SIMD-0296),
-- against the 1232 of earlier formats.
maxTransactionSize :: Int
maxTransactionSize = 4096

-- | One ed25519 signature.
signatureSize :: Int
signatureSize = 64

minHeapSize :: Word32
minHeapSize = 32 * 1024

maxHeapSize :: Word32
maxHeapSize = 256 * 1024
