{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden and behaviour tests for v1 (SIMD-0385) message compilation.
--
-- The golden vector is a real transaction recorded from devnet
-- (@test\/fixtures\/v1_transactions.json@), not a hand-written sample: its
-- inputs are fed back through the builder and the result must match the
-- bytes the network actually carried. That is how the layout stays pinned
-- while @tools\/fixture-gen@ remains on its solana-sdk 1.18 pin, which
-- predates the format and so cannot generate a vector for it.
--
-- One recorded transaction cannot pin everything, so the cases after it
-- cover what it structurally cannot: it has a single instruction (so it
-- cannot tell all-headers-then-all-payloads apart from interleaved), a
-- single signer (so it cannot tell signature slots apart), and no heap
-- request. Each limit is pinned by a pair of cases straddling it rather
-- than by matching a number in an error message.
module Test.Core.MessageV1 (tests) where

import Data.Binary (Binary (..), Word8)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction (AccountMeta (..), CompileException (..), Instruction, mkInstruction)
import Network.Solana.Core.MessageV1
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

-- | Instruction data passed through verbatim; 'mkInstruction' encodes its
-- payload with 'Binary', and the standard instances for 'BS.ByteString' and
-- lists prepend a length the wire format must not carry.
newtype RawData = RawData BS.ByteString

instance Binary RawData where
  put (RawData bs) = putByteString bs
  get = fail "RawData: decoding not used"

------------------------------------------------------------------------------------------------
-- The recorded devnet transaction, as its own inputs.
------------------------------------------------------------------------------------------------

-- | Fee payer: the single required signer, writable.
devnetPayer :: SolanaPublicKey
devnetPayer = unsafeSolanaPublicKey "4QVtKaZkDhEwKszzJWa7LJPZBFkkcgUVRG4xE3fkWNGt"

-- | The one writable unsigned account.
devnetWritable :: SolanaPublicKey
devnetWritable = unsafeSolanaPublicKey "55gx5ra96qkYXXMGShkVjwQz1k4j9bS26HhdksFa9buL"

-- | The invoked program.
devnetProgram :: SolanaPublicKey
devnetProgram = unsafeSolanaPublicKey "MktiyFj9mVdLQgj45AXvJdoaHvF9PkuU1UmA2uwPJWM"

devnetClock :: SolanaPublicKey
devnetClock = unsafeSolanaPublicKey "SysvarC1ock11111111111111111111111111111111"

devnetBlockhash :: BlockHash
devnetBlockhash = BlockHash (requireBase58 "4iSS7zQNeY8BTSWZ9VRMBo8B17AyijyLDD8C2EycoW19")

-- | The recorded instruction's 90 data bytes.
devnetInstructionData :: BS.ByteString
devnetInstructionData =
  requireBase58
    "2isQdwGHZhqBC7JtD5qgwY3fpHY2d1ciRmLgt9KN5x75VY2UxdpWv67RMmcvfJCqk5SAeAV4zDVqkkrgrJga5gREAVsRKJUhgqq5ZxqV2rNZhRkuobuQtQubPy9"

-- | @priorityFee = 0@, @computeUnitLimit = 324@,
-- @loadedAccountsDataSizeLimit = 43008@, no heap request -- config mask 0xF.
devnetConfig :: TransactionConfig
devnetConfig =
  TransactionConfig
    { tcPriorityFee = Just 0,
      tcComputeUnitLimit = Just 324,
      tcLoadedAccountsDataSizeLimit = Just 43008,
      tcHeapSize = Nothing
    }

requireBase58 :: String -> BS.ByteString
requireBase58 s = maybe (error ("bad base58: " <> s)) id (fromBase58String s)

tests :: TestTree
tests =
  testGroup
    "v1 message compilation (SIMD-0385)"
    [ testCase "message bytes match a transaction recorded from devnet" $ do
        (expectedMessage, signature) <- devnetGolden
        let ix =
              mkInstruction
                devnetProgram
                [ AccountMeta devnetPayer True True,
                  AccountMeta devnetClock False False,
                  AccountMeta devnetWritable False True
                ]
                (RawData devnetInstructionData)
        bytes <- requireRight (compileV1Message devnetPayer devnetBlockhash [ix] devnetConfig)
        bytes @?= expectedMessage
        -- Cross-check the fixture against itself: the bytes the golden
        -- compares to must be exactly what the recorded signature signs, so
        -- a re-recording with a different signer count cannot silently
        -- shift the split point.
        assertBool
          "the recorded signature verifies over the recorded message"
          (dverify devnetPayer expectedMessage signature),
      testGroup "wire layout the recorded transaction cannot pin" layoutTests,
      testGroup "configuration mask" maskTests,
      testGroup "signatures" signatureTests,
      testGroup "limits" limitTests,
      testGroup "refused as unusable" unusableTests
    ]

-- | The recorded transaction, split into the message and its one signature
-- using the signer count the header itself declares (byte 1) rather than a
-- hardcoded offset.
devnetGolden :: IO (BS.ByteString, SolanaSignature)
devnetGolden = do
  fs <- loadFixtures "test/fixtures/v1_transactions.json"
  let wire = requireFixture "devnet-v1-transaction" fs
      signers = fromIntegral (BS.index wire 1)
      msgLen = BS.length wire - 64 * signers
  signers @?= (1 :: Int)
  pure (BS.take msgLen wire, unsafeSigFromString (toBase58String (BS.drop msgLen wire)))

------------------------------------------------------------------------------------------------
-- Layout
------------------------------------------------------------------------------------------------

layoutTests :: [TestTree]
layoutTests =
  [ testCase "all instruction headers precede all payloads" $ do
      -- Three instructions with distinct account counts (1, 2, 3) and
      -- distinct data lengths (1, 2, 3). Interleaving each header with its
      -- own payload -- the legacy and v0 layout, and so the likeliest wrong
      -- edit -- puts an account index where the second header belongs, and
      -- this assertion fails.
      let ix n dat =
            mkInstruction
              layoutProgram
              (AccountMeta payerPk True True : [AccountMeta k False True | k <- take (n - 1) [keyA, keyB]])
              (RawData (BS.pack dat))
          instrs = [ix 1 [0xA0], ix 2 [0xB0, 0xB1], ix 3 [0xC0, 0xC1, 0xC2]]
      bytes <- requireRight (compileV1Message payerPk fixedBlockhash instrs minimalConfig)
      -- payer, keyA, keyB are accounts 0..2 and the program is account 3.
      let headers = [3, 1, 1, 0, 3, 2, 2, 0, 3, 3, 3, 0]
          payloads = [0, 0xA0, 0, 1, 0xB0, 0xB1, 0, 1, 2, 0xC0, 0xC1, 0xC2]
          expected = BS.pack (headers <> payloads)
      BS.drop (BS.length bytes - BS.length expected) bytes @?= expected
  ]

------------------------------------------------------------------------------------------------
-- Configuration mask
------------------------------------------------------------------------------------------------

-- | The mask bytes and the config values sit immediately after the
-- addresses; @addressesEnd@ is where the values begin.
maskTests :: [TestTree]
maskTests =
  [ maskCase "compute-unit and loaded-accounts limits only" minimalConfig 0b1100 8,
    maskCase "with a priority fee (two bits, eight bytes)" minimalConfig {tcPriorityFee = Just 7} 0b1111 16,
    maskCase "with a heap request" minimalConfig {tcHeapSize = Just (64 * 1024)} 0b11100 12,
    maskCase
      "with every field set"
      minimalConfig {tcPriorityFee = Just 7, tcHeapSize = Just (64 * 1024)}
      0b11111
      20,
    testCase "values are little-endian and in field order" $ do
      let full =
            TransactionConfig
              { tcPriorityFee = Just 0x1122334455667788,
                tcComputeUnitLimit = Just 0x0A0B0C0D,
                tcLoadedAccountsDataSizeLimit = Just 0x01020304,
                tcHeapSize = Just (64 * 1024)
              }
      bytes <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] full)
      assertEqual
        "priority fee u64, then three u32s"
        ( BS.pack
            [ 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11,
              0x0d, 0x0c, 0x0b, 0x0a,
              0x04, 0x03, 0x02, 0x01,
              0x00, 0x00, 0x01, 0x00
            ]
        )
        (BS.take 20 (BS.drop (valuesOffset bytes) bytes))
  ]

-- | Assert a configuration's mask bytes and the number of value bytes it
-- appends. The byte count is what distinguishes the two-bit, eight-byte
-- priority fee from the one-bit, four-byte fields.
maskCase :: String -> TransactionConfig -> Word -> Int -> TestTree
maskCase name config mask valueBytes =
  testCase name $ do
    bytes <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] config)
    assertEqual "mask" (BS.pack [fromIntegral mask, fromIntegral (mask `div` 256), 0, 0]) (BS.take 4 (BS.drop 4 bytes))
    bare <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] minimalConfig)
    -- Every configuration here differs from the baseline only in its value
    -- bytes, so the length difference is exactly the extra values.
    BS.length bytes - BS.length bare @?= valueBytes - 8

-- | Where the config values start: the 42-byte fixed header followed by the
-- address array, whose length byte sits at offset 41.
valuesOffset :: BS.ByteString -> Int
valuesOffset bytes = 42 + 32 * fromIntegral (BS.index bytes 41)

------------------------------------------------------------------------------------------------
-- Signatures
------------------------------------------------------------------------------------------------

signatureTests :: [TestTree]
signatureTests =
  [ testCase "one signature follows the message, unprefixed, and verifies over it" $ do
      msg <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] minimalConfig)
      signed <- requireRight (newV1TransactionIntent payerPk [payerSk] [simpleInstruction] minimalConfig fixedBlockhash)
      let wire = fromBase64String signed
      BS.length wire @?= BS.length msg + 64
      assertEqual "message comes first" msg (BS.take (BS.length msg) wire)
      assertBool
        "signature verifies over the message bytes"
        (dverify payerPk msg (sigAt msg wire 0)),
      testCase "two signatures occupy their signers' account slots, in message order" $ do
        -- co-signer is seeded writable so both are writable signers; the
        -- payer is pinned to account 0 and the other follows.
        let ix = mkInstruction layoutProgram [AccountMeta payerPk True True, AccountMeta coPk True True] ()
        msg <- requireRight (compileV1Message payerPk fixedBlockhash [ix] minimalConfig)
        signed <- requireRight (newV1TransactionIntent payerPk [payerSk, coSk] [ix] minimalConfig fixedBlockhash)
        let wire = fromBase64String signed
        BS.length wire @?= BS.length msg + 128
        assertBool "slot 0 is the fee payer's" (dverify payerPk msg (sigAt msg wire 0))
        assertBool "slot 1 is the co-signer's" (dverify coPk msg (sigAt msg wire 1))
        assertBool "slot 0 is not the co-signer's" (not (dverify coPk msg (sigAt msg wire 0)))
        -- The keys are matched to the message's signers, so the order they
        -- are supplied in cannot change the bytes.
        reversed <-
          requireRight (newV1TransactionIntent payerPk [coSk, payerSk] [ix] minimalConfig fixedBlockhash)
        reversed @?= signed,
      testCase "a key for no required signer is refused" $
        assertRejected
          "unused signing key"
          (newV1TransactionIntent payerPk [payerSk, coSk] [simpleInstruction] minimalConfig fixedBlockhash),
      testCase "a missing signer is refused" $ do
        let ix = mkInstruction layoutProgram [AccountMeta payerPk True True, AccountMeta coPk True True] ()
        assertRejected "missing signer" (newV1TransactionIntent payerPk [payerSk] [ix] minimalConfig fixedBlockhash)
  ]

-- | The @n@th 64-byte signature slot of a signed transaction.
sigAt :: BS.ByteString -> BS.ByteString -> Int -> SolanaSignature
sigAt msg wire n =
  unsafeSigFromString (toBase58String (BS.take 64 (BS.drop (BS.length msg + 64 * n) wire)))

------------------------------------------------------------------------------------------------
-- Limits, each pinned by a pair of cases straddling it
------------------------------------------------------------------------------------------------

limitTests :: [TestTree]
limitTests =
  [ testCase "64 addresses compile, 65 do not" $ do
      _ <- requireRight (compileV1Message payerPk fixedBlockhash [wideInstruction 62] minimalConfig)
      assertRejected "addresses" (compileV1Message payerPk fixedBlockhash [wideInstruction 63] minimalConfig),
    testCase "64 instructions compile, 65 do not" $ do
      _ <- requireRight (compileV1Message payerPk fixedBlockhash (replicate 64 simpleInstruction) minimalConfig)
      assertRejected
        "instructions"
        (compileV1Message payerPk fixedBlockhash (replicate 65 simpleInstruction) minimalConfig),
    testCase "255 accounts in one instruction compile, 256 do not" $ do
      _ <- requireRight (compileV1Message payerPk fixedBlockhash [repeatedAccounts 255] minimalConfig)
      assertRejected
        "accounts in one instruction"
        (compileV1Message payerPk fixedBlockhash [repeatedAccounts 256] minimalConfig),
    testCase "12 signatures compile, 13 do not" $ do
      _ <- requireRight (compileV1Message payerPk fixedBlockhash [manySigners 12] minimalConfig)
      assertRejected
        "required signatures"
        (compileV1Message payerPk fixedBlockhash [manySigners 13] minimalConfig),
    testCase "a transaction of exactly 4096 bytes compiles, 4097 does not" $ do
      bytes <- requireRight (compileV1Message payerPk fixedBlockhash [bigDataInstruction atCap] minimalConfig)
      -- One required signer, so the signed transaction is 64 bytes longer.
      BS.length bytes + 64 @?= 4096
      assertRejected
        "4096"
        (compileV1Message payerPk fixedBlockhash [bigDataInstruction (atCap + 1)] minimalConfig),
    testCase "the size cap counts one signature per required signer" $ do
      -- With 12 signers the signature array is 768 bytes, so a message that
      -- would fit comfortably on its own must still be refused. A size
      -- check that ignored the signer count would accept this.
      let twelve n = mkInstruction layoutProgram (signerMetas 12) (RawData (BS.replicate n 7))
      big <- pure (compileV1Message payerPk fixedBlockhash [twelve 3000] minimalConfig)
      assertRejected "4096" big
      small <- requireRight (compileV1Message payerPk fixedBlockhash [twelve 2000] minimalConfig)
      assertBool "the accepted one leaves room for 12 signatures" (BS.length small + 12 * 64 <= 4096),
    testCase "the heap request's bounds are inclusive" $ do
      _ <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] (heap 32768))
      _ <- requireRight (compileV1Message payerPk fixedBlockhash [simpleInstruction] (heap 262144))
      assertRejected "below the minimum" (compileV1Message payerPk fixedBlockhash [simpleInstruction] (heap 31744))
      assertRejected "above the maximum" (compileV1Message payerPk fixedBlockhash [simpleInstruction] (heap 263168))
      assertRejected
        "multiple of 1024"
        (compileV1Message payerPk fixedBlockhash [simpleInstruction] (heap 33000))
  ]
  where
    heap n = minimalConfig {tcHeapSize = Just n}

-- | The instruction data length that makes the signed transaction exactly
-- 4096 bytes: the cap, less one signature, the 42-byte fixed header, three
-- 32-byte addresses, eight config bytes, one instruction header and the two
-- account indices.
atCap :: Int
atCap = 4096 - 64 - 42 - 3 * 32 - 8 - 4 - 2

------------------------------------------------------------------------------------------------
-- Refused as unusable
------------------------------------------------------------------------------------------------

unusableTests :: [TestTree]
unusableTests =
  [ testCase "an empty instruction list" $
      assertRejected "no instructions" (compileV1Message payerPk fixedBlockhash [] minimalConfig),
    testCase "a config requesting nothing" $
      assertRejected
        "compute-unit limit of 0"
        (compileV1Message payerPk fixedBlockhash [simpleInstruction] emptyTransactionConfig),
    testCase "an absent compute-unit limit" $
      assertRejected
        "compute-unit limit of 0"
        ( compileV1Message
            payerPk
            fixedBlockhash
            [simpleInstruction]
            minimalConfig {tcComputeUnitLimit = Nothing}
        ),
    testCase "an explicitly zero compute-unit limit" $
      assertRejected
        "compute-unit limit of 0"
        ( compileV1Message
            payerPk
            fixedBlockhash
            [simpleInstruction]
            minimalConfig {tcComputeUnitLimit = Just 0}
        ),
    testCase "an absent loaded-accounts data size limit" $
      assertRejected
        "loaded-accounts data size limit of 0"
        ( compileV1Message
            payerPk
            fixedBlockhash
            [simpleInstruction]
            minimalConfig {tcLoadedAccountsDataSizeLimit = Nothing}
        ),
    testCase "an explicitly zero loaded-accounts data size limit" $
      assertRejected
        "loaded-accounts data size limit of 0"
        ( compileV1Message
            payerPk
            fixedBlockhash
            [simpleInstruction]
            minimalConfig {tcLoadedAccountsDataSizeLimit = Just 0}
        ),
    testCase "the fee payer named as a program id" $
      assertRejected
        "cannot be a program id"
        ( compileV1Message
            layoutProgram
            fixedBlockhash
            [mkInstruction layoutProgram [AccountMeta payerPk False True] ()]
            minimalConfig
        )
  ]

------------------------------------------------------------------------------------------------
-- Fixtures and helpers
------------------------------------------------------------------------------------------------

-- | The smallest configuration the builder accepts: both effectively
-- required fields set, nothing else.
minimalConfig :: TransactionConfig
minimalConfig =
  emptyTransactionConfig
    { tcComputeUnitLimit = Just 200000,
      tcLoadedAccountsDataSizeLimit = Just 131072
    }

-- | An instruction touching @n@ distinct writable accounts, so the compiled
-- message carries @n + 2@ addresses (the payer and the program).
wideInstruction :: Int -> Instruction
wideInstruction n =
  mkInstruction
    layoutProgram
    (AccountMeta payerPk True True : [AccountMeta (numberedKey i) False True | i <- [1 .. n]])
    ()

-- | An instruction naming the same account @n@ times, so its account-index
-- count can exceed the message's address count.
repeatedAccounts :: Int -> Instruction
repeatedAccounts n =
  mkInstruction layoutProgram (AccountMeta payerPk True True : replicate (n - 1) (AccountMeta keyA False True)) ()

-- | An instruction requiring @n@ signatures.
manySigners :: Int -> Instruction
manySigners n = mkInstruction layoutProgram (signerMetas n) ()

signerMetas :: Int -> [AccountMeta]
signerMetas n = AccountMeta payerPk True True : [AccountMeta (numberedKey i) True True | i <- [1 .. n - 1]]

-- | An instruction carrying @n@ data bytes and two account indices.
bigDataInstruction :: Int -> Instruction
bigDataInstruction n =
  mkInstruction
    layoutProgram
    [AccountMeta payerPk True True, AccountMeta keyA False True]
    (RawData (BS.replicate n 7))

simpleInstruction :: Instruction
simpleInstruction = SP.transfer payerPk keyA 1

payerKeys :: (SolanaPublicKey, SolanaPrivateKey)
payerKeys = seededKeypair 1

coSignerKeys :: (SolanaPublicKey, SolanaPrivateKey)
coSignerKeys = seededKeypair 2

payerPk :: SolanaPublicKey
payerPk = fst payerKeys

payerSk :: SolanaPrivateKey
payerSk = snd payerKeys

coPk :: SolanaPublicKey
coPk = fst coSignerKeys

coSk :: SolanaPrivateKey
coSk = snd coSignerKeys

seededKeypair :: Word8 -> (SolanaPublicKey, SolanaPrivateKey)
seededKeypair seed =
  case createSolanaKeypairFromSeed (BS.replicate 32 seed) of
    Just kp -> kp
    Nothing -> error ("failed to derive keypair from seed " <> show seed)

-- | Two ordinary accounts, chosen so that their raw bytes sort in this
-- order (the layout test depends on their account indices).
keyA, keyB :: SolanaPublicKey
keyA = unsafeSolanaPublicKeyRaw (5 : replicate 31 0)
keyB = unsafeSolanaPublicKeyRaw (6 : replicate 31 0)

-- | A program id sorting after 'keyA' and 'keyB', so it is the last
-- address.
layoutProgram :: SolanaPublicKey
layoutProgram = unsafeSolanaPublicKeyRaw (9 : replicate 31 0)

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

-- | A distinct 32-byte key per number, for large key sets.
numberedKey :: Int -> SolanaPublicKey
numberedKey b = unsafeSolanaPublicKeyRaw (3 : fromIntegral (b `div` 256) : fromIntegral (b `mod` 256) : replicate 29 0)

requireRight :: Either CompileException a -> IO a
requireRight = either (assertFailure . show) pure

-- | Assert a compile failed, and that its message names the reason -- so a
-- rejection for an unrelated reason cannot pass for the right one.
assertRejected :: String -> Either CompileException a -> Assertion
assertRejected expected = \case
  Right _ -> assertFailure ("expected rejection mentioning " <> show expected)
  Left err ->
    let msg = compileExceptionMessage err
     in assertBool ("expected " <> show expected <> " in: " <> msg) (expected `isInfixOf` msg)

compileExceptionMessage :: CompileException -> String
compileExceptionMessage = \case
  MissingIndex m -> m
  LimitExceeded m -> m
  InvalidMessage m -> m
