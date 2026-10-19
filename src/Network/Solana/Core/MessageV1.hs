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
--
-- Unlike the legacy and v0 builders, both entry points here take the fee
-- payer explicitly. The reference implementation's only v1 compile path
-- does the same (@v1::Message::try_compile_with_config(payer, ..)@), and
-- deriving the payer implicitly from the instruction list is a trap this
-- format makes expensive: the priority fee travels inside the message, so
-- whoever lands in account 0 pays it.
module Network.Solana.Core.MessageV1
  ( TransactionConfig (..),
    emptyTransactionConfig,
    compileV1Message,
    newV1TransactionIntent,
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
-- sets its bit in the message's configuration mask and appends its value.
--
-- __An absent field does not mean \"the usual default\"__: SIMD-0385 uses
-- the /minimum/ value instead, which for two of these fields is zero. A
-- transaction requesting zero compute units, or zero bytes of loaded
-- account data, is still accepted into a block and still charges its fee
-- payer -- it simply fails without doing anything. 'compileV1Message'
-- therefore refuses to build one; see 'emptyTransactionConfig'.
--
-- @ComputeBudget@ instructions placed in a v1 transaction are ignored for
-- configuration purposes -- they still execute, as successful no-ops that
-- consume compute units -- so resource requests must be made here.
data TransactionConfig = TransactionConfig
  { -- | Total priority fee, in lamports; absent means 0.
    --
    -- __Not the same unit as @ComputeBudget@__: @SetComputeUnitPrice@ takes
    -- micro-lamports /per compute unit/, while this is a flat total. A
    -- value carried across unconverted overpays by a factor of up to a
    -- million, and the network accepts it. To port one, multiply the price
    -- by the compute-unit limit and divide by 1e6.
    tcPriorityFee :: Maybe Word64,
    -- | Maximum compute units the transaction may consume. Absent means 0,
    -- which cannot execute anything, so this is effectively required.
    tcComputeUnitLimit :: Maybe Word32,
    -- | Maximum bytes of account data the transaction may load. Absent
    -- means 0 -- not the 64 MiB earlier formats default to -- so this is
    -- effectively required too.
    tcLoadedAccountsDataSizeLimit :: Maybe Word32,
    -- | Heap size in bytes; absent means 32 KiB. When given it must be a
    -- multiple of 1024 and within [32768, 262144]; 'compileV1Message'
    -- rejects anything else.
    tcHeapSize :: Maybe Word32
  }
  deriving (Show, Eq, Generic)

-- | A configuration requesting nothing at all.
--
-- This is a base for record update, not a usable configuration:
-- 'compileV1Message' rejects it, because a v1 message with no compute-unit
-- limit and no loaded-accounts limit requests zero of both and so is
-- guaranteed to be charged for and then fail (see 'TransactionConfig').
-- Fill in at least those two fields:
--
-- > emptyTransactionConfig
-- >   { tcComputeUnitLimit = Just 200000,
-- >     tcLoadedAccountsDataSizeLimit = Just 131072
-- >   }
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

-- | Compile a v1 message for the given fee payer: assemble the account keys
-- exactly as the legacy and v0 builders do (privilege union, then canonical
-- ordering, via 'mkNewMessageWithPayer', which seeds the payer as a writable
-- signer and pins it to account 0), then serialize them in the v1 layout
-- behind the @129@ version byte. Naming the payer also supports sponsored
-- fees: it need not appear in any instruction.
--
-- Fails with a 'CompileException' when the result would not be a usable v1
-- message:
--
-- * 'LimitExceeded' -- more than 64 addresses, more than 64 instructions,
--   more than 12 required signatures, an instruction with more than 255
--   accounts or more than 65535 data bytes, or a transaction that would
--   exceed 4096 bytes once signed.
-- * 'InvalidMessage' -- no instructions at all, the fee payer used as a
--   program id, a compute-unit or loaded-accounts request of zero (see
--   'TransactionConfig'), or a heap request that is not a multiple of 1024
--   within [32768, 262144].
compileV1Message ::
  SolanaPublicKey -> BlockHash -> [Instruction] -> TransactionConfig -> Either CompileException BS.ByteString
compileV1Message payer bh instructions =
  compileV1FromLegacy (mkNewMessageWithPayer payer bh instructions)

-- | Build and sign a v1 transaction for the given fee payer, returning it
-- Base64-encoded. A v1 transaction puts the message first and the
-- signatures last, as a bare sequence of 64-byte signatures with no length
-- prefix -- the reverse of the legacy and v0 envelopes, and the reason the
-- signature count has to match the message's required signers exactly:
-- there is no count on the wire to describe a mismatch.
--
-- The given private keys may be listed in any order. They are matched
-- against the compiled message's required signers and placed in message
-- order automatically, so a key for the wrong account, a missing key or a
-- spare key is a 'MissingIndex' failure rather than a silently malformed
-- transaction.
newV1TransactionIntent ::
  SolanaPublicKey ->
  [SolanaPrivateKey] ->
  [Instruction] ->
  TransactionConfig ->
  BlockHash ->
  Either CompileException String
newV1TransactionIntent payer signingKeys instructions config bh = do
  let legacy = mkNewMessageWithPayer payer bh instructions
  msg <- compileV1FromLegacy legacy config
  orderedKeys <- orderSigningKeys legacy signingKeys
  return $ toBase64String (msg <> signaturesOver msg orderedKeys)

------------------------------------------------------------------------------------------------

-- * Internal helpers

------------------------------------------------------------------------------------------------

-- | Serialize an assembled legacy message in the v1 layout.
--
-- The header's @numReadonlySignedAccounts < numRequiredSignatures@ sanitize
-- rule needs no check here: every caller routes through
-- 'mkNewMessageWithPayer', which puts the payer in the writable-signer
-- section and never demotes it, so that section is never empty.
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
--
-- The compute-unit and loaded-accounts bits are only distinguishable from
-- each other when exactly one of the two is present, which 'checkLimits'
-- forbids -- so no test can pin them apart while that guard stands. Relax
-- the guard and they need pinning.
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

-- | Everything the builder can check before serializing: the format's count
-- caps, the sanitize rules reachable from here, and the resource requests
-- that would make the transaction pay to do nothing.
checkLimits :: Message -> [CompiledInstruction] -> TransactionConfig -> Either CompileException ()
checkLimits legacy compiled TransactionConfig {..} = do
  limit "addresses" maxAddresses (length (mAccountKeys legacy))
  limit "instructions" maxInstructions (length compiled)
  limit "required signatures" maxSignatures (fromIntegral (numRequiredSignatures (mHeader legacy)))
  checkNonEmpty
  mapM_ checkInstruction compiled
  checkRequest "compute-unit limit" tcComputeUnitLimit
  checkRequest "loaded-accounts data size limit" tcLoadedAccountsDataSizeLimit
  mapM_ checkHeapSize tcHeapSize
  where
    limit what cap actual
      | actual <= cap = Right ()
      | otherwise =
          Left
            ( LimitExceeded
                ("v1 message has " <> show actual <> " " <> what <> ", more than the " <> show cap <> " allowed")
            )
    checkNonEmpty
      | null compiled = Left (InvalidMessage "v1 message has no instructions, so it would pay a fee to do nothing")
      | otherwise = Right ()
    checkInstruction CompiledInstruction {..} = do
      limit "accounts in one instruction" maxInstructionAccounts (length (unCompact ciAccounts))
      limit "data bytes in one instruction" maxInstructionData (length (unCompact ciData))
      if ciProgramIdIndex == 0
        then Left (InvalidMessage "v1 instruction invokes account 0, but the fee payer cannot be a program id")
        else Right ()
    -- Absent or zero are the same request on the wire, and both guarantee
    -- an included, charged, failing transaction -- so both are refused.
    checkRequest what request
      | request `elem` [Nothing, Just 0] =
          Left
            ( InvalidMessage
                ( "v1 message requests a "
                    <> what
                    <> " of 0, which cannot execute anything; set it in the TransactionConfig"
                )
            )
      | otherwise = Right ()
    checkHeapSize n
      | n `mod` 1024 /= 0 =
          Left (InvalidMessage ("v1 heap size " <> show n <> " is not a multiple of 1024"))
      | n < minHeapSize =
          Left (InvalidMessage ("v1 heap size " <> show n <> " is below the minimum of " <> show minHeapSize))
      | n > maxHeapSize =
          Left (InvalidMessage ("v1 heap size " <> show n <> " is above the maximum of " <> show maxHeapSize))
      | otherwise = Right ()

-- | Reject a message whose transaction -- the message plus one 64-byte
-- signature per required signer -- would exceed the v1 size cap. Checked on
-- the message, since the header fixes the signature count and
-- 'newV1TransactionIntent' signs exactly that many.
checkTransactionSize :: Message -> BS.ByteString -> Either CompileException ()
checkTransactionSize legacy bytes
  | total <= maxTransactionSize = Right ()
  | otherwise =
      Left
        ( LimitExceeded
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
