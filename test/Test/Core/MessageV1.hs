{-# LANGUAGE OverloadedStrings #-}

-- | Golden and behaviour tests for v1 (SIMD-0385) message compilation.
--
-- The golden vector is a real transaction recorded from devnet
-- (@test\/fixtures\/v1_transactions.json@), not a hand-written sample: its
-- inputs are fed back through the builder and the result must match the
-- bytes the network actually carried, which is the only way to pin the
-- layout of a format this module is the first Haskell implementation of.
module Test.Core.MessageV1 (tests) where

import Data.Binary (Binary (..))
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
devnetBlockhash =
  BlockHash (requireBase58 "4iSS7zQNeY8BTSWZ9VRMBo8B17AyijyLDD8C2EycoW19")

-- | The recorded instruction's 90 data bytes.
devnetInstructionData :: BS.ByteString
devnetInstructionData =
  requireBase58
    "2isQdwGHZhqBC7JtD5qgwY3fpHY2d1ciRmLgt9KN5x75VY2UxdpWv67RMmcvfJCqk5SAeAV4zDVqkkrgrJga5gREAVsRKJUhgqq5ZxqV2rNZhRkuobuQtQubPy9"

requireBase58 :: String -> BS.ByteString
requireBase58 s = maybe (error ("bad base58: " <> s)) id (fromBase58String s)

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

tests :: TestTree
tests =
  testGroup
    "v1 message compilation (SIMD-0385)"
    [ testCase "v1 message bytes match a transaction recorded from devnet" $ do
        fs <- loadFixtures "test/fixtures/v1_transactions.json"
        let wire = requireFixture "devnet-v1-transaction" fs
            -- One required signer, so the trailing signature array is 64 bytes.
            expectedMessage = BS.take (BS.length wire - 64) wire
            ix =
              mkInstruction
                devnetProgram
                [ AccountMeta devnetPayer True True,
                  AccountMeta devnetClock False False,
                  AccountMeta devnetWritable False True
                ]
                (RawData devnetInstructionData)
        case compileV1Message devnetBlockhash [ix] devnetConfig of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= expectedMessage,
      testCase "signatures follow the message, unprefixed, and verify over it" $ do
        let (payerPk, payerSk) = payerKeys
            ix = SP.transfer payerPk recipientPk 1000000
        msg <- requireRight (compileV1Message fixedBlockhash [ix] emptyTransactionConfig)
        signed <- requireRight (newV1TransactionIntent [payerSk] [ix] emptyTransactionConfig fixedBlockhash)
        let wire = fromBase64String signed
        BS.length wire @?= BS.length msg + 64
        assertEqual "message comes first" msg (BS.take (BS.length msg) wire)
        let sig = unsafeSigFromString (toBase58String (BS.drop (BS.length msg) wire))
        assertBool "signature verifies over the message bytes" (dverify payerPk msg sig),
      testCase "the fee payer variant orders signatures and matches the ordered one" $ do
        let (payerPk, payerSk) = payerKeys
            ix = SP.transfer payerPk recipientPk 1000000
        ordered <- requireRight (newV1TransactionIntent [payerSk] [ix] emptyTransactionConfig fixedBlockhash)
        named <-
          requireRight
            (newV1TransactionIntentWithPayer payerPk [payerSk] [ix] emptyTransactionConfig fixedBlockhash)
        named @?= ordered,
      testCase "an empty config sets no mask bits and appends no values" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1
        bare <- requireRight (compileV1Message fixedBlockhash [ix] emptyTransactionConfig)
        -- Mask occupies the four bytes after the version byte and the 3-byte header.
        BS.take 4 (BS.drop 4 bare) @?= BS.replicate 4 0,
      testCase "every config field sets its bit and appends its value in field order" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1
            full =
              TransactionConfig
                { tcPriorityFee = Just 0x1122334455667788,
                  tcComputeUnitLimit = Just 0x0A0B0C0D,
                  tcLoadedAccountsDataSizeLimit = Just 0x01020304,
                  tcHeapSize = Just (64 * 1024)
                }
        bytes <- requireRight (compileV1Message fixedBlockhash [ix] full)
        -- priority fee occupies two mask bits, so all four fields set 0b11111.
        assertEqual "mask" (BS.pack [0x1f, 0, 0, 0]) (BS.take 4 (BS.drop 4 bytes))
        let addressCount = fromIntegral (BS.index bytes 41)
            valuesAt = 42 + addressCount * 32
        assertEqual
          "values, little-endian, in field order"
          (BS.pack [0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x0d, 0x0c, 0x0b, 0x0a, 0x04, 0x03, 0x02, 0x01, 0x00, 0x00, 0x01, 0x00])
          (BS.take 20 (BS.drop valuesAt bytes)),
      testGroup
        "rejected by the v1 limits"
        [ testCase "more than 64 addresses" $
            assertRejected "addresses" (compileV1Message fixedBlockhash [wideInstruction 64] emptyTransactionConfig),
          testCase "exactly 64 addresses compile" $
            requireRight (compileV1Message fixedBlockhash [wideInstruction 62] emptyTransactionConfig) >> pure (),
          testCase "more than 64 instructions" $
            assertRejected
              "instructions"
              (compileV1Message fixedBlockhash (replicate 65 (SP.transfer (fst payerKeys) recipientPk 1)) emptyTransactionConfig),
          testCase "a heap request that is not a multiple of 1024" $
            assertRejected
              "multiple of 1024"
              (compileV1Message fixedBlockhash [simpleInstruction] emptyTransactionConfig {tcHeapSize = Just 33000}),
          testCase "a heap request below the 32 KiB minimum" $
            assertRejected
              "outside"
              (compileV1Message fixedBlockhash [simpleInstruction] emptyTransactionConfig {tcHeapSize = Just 16384}),
          testCase "a heap request above the 256 KiB maximum" $
            assertRejected
              "outside"
              (compileV1Message fixedBlockhash [simpleInstruction] emptyTransactionConfig {tcHeapSize = Just (257 * 1024)}),
          testCase "more than 255 accounts in one instruction" $
            -- Reachable with only three addresses: repeated metas each
            -- compile to their own account index.
            assertRejected
              "accounts in one instruction"
              ( compileV1Message
                  fixedBlockhash
                  [ mkInstruction
                      dummyProgramId
                      (AccountMeta (fst payerKeys) True True : replicate 256 (AccountMeta recipientPk False True))
                      ()
                  ]
                  emptyTransactionConfig
              ),
          testCase "a transaction larger than 4096 bytes" $
            assertRejected
              "4096"
              (compileV1Message fixedBlockhash [bigDataInstruction 4096] emptyTransactionConfig),
          testCase "a transaction just inside 4096 bytes compiles" $ do
            bytes <- requireRight (compileV1Message fixedBlockhash [bigDataInstruction 3900] emptyTransactionConfig)
            assertBool "under the cap once signed" (BS.length bytes + 64 <= 4096)
        ]
    ]

-- | An instruction touching @n@ distinct writable accounts, so the compiled
-- message carries @n + 2@ addresses (the payer and the program).
wideInstruction :: Int -> Instruction
wideInstruction n =
  mkInstruction
    dummyProgramId
    (AccountMeta (fst payerKeys) True True : [AccountMeta (numberedKey i) False True | i <- [1 .. n]])
    ()

-- | An instruction carrying @n@ data bytes.
bigDataInstruction :: Int -> Instruction
bigDataInstruction n =
  mkInstruction dummyProgramId [AccountMeta (fst payerKeys) True True] (RawData (BS.replicate n 7))

simpleInstruction :: Instruction
simpleInstruction = SP.transfer (fst payerKeys) recipientPk 1

payerKeys :: (SolanaPublicKey, SolanaPrivateKey)
payerKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just kp -> kp
    Nothing -> error "failed to derive payer keypair from seed"

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

dummyProgramId :: SolanaPublicKey
dummyProgramId = unsafeSolanaPublicKeyRaw (replicate 32 40)

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

-- | A distinct 32-byte key per number, for large key sets.
numberedKey :: Int -> SolanaPublicKey
numberedKey b = unsafeSolanaPublicKeyRaw (3 : fromIntegral (b `div` 256) : fromIntegral (b `mod` 256) : replicate 29 0)

requireRight :: Either CompileException a -> IO a
requireRight = either (assertFailure . show) pure

-- | Assert a compile failed, and that its message names the limit hit --
-- so a rejection for an unrelated reason cannot pass for the right one.
assertRejected :: String -> Either CompileException BS.ByteString -> Assertion
assertRejected expected result = case result of
  Right _ -> assertFailure ("expected rejection mentioning " <> show expected)
  Left (MissingIndex msg) ->
    assertBool ("expected " <> show expected <> " in: " <> msg) (expected `isInfixOf` msg)
