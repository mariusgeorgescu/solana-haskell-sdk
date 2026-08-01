{-# LANGUAGE OverloadedStrings #-}

module Test.Core.VersionedMessage (tests) where

import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction (AccountMeta (..), CompileException (..), mkInstruction)
import Network.Solana.Core.VersionedMessage
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

payerKeys :: (SolanaPublicKey, SolanaPrivateKey)
payerKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just kp -> kp
    Nothing -> error "failed to derive payer keypair from seed"

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

addr29 :: SolanaPublicKey
addr29 = unsafeSolanaPublicKeyRaw (replicate 32 29)

addr30 :: SolanaPublicKey
addr30 = unsafeSolanaPublicKeyRaw (replicate 32 30)

tableKey :: SolanaPublicKey
tableKey = unsafeSolanaPublicKeyRaw (replicate 32 31)

unrelatedKey :: SolanaPublicKey
unrelatedKey = unsafeSolanaPublicKeyRaw (replicate 32 99)

otherTableKey :: SolanaPublicKey
otherTableKey = unsafeSolanaPublicKeyRaw (replicate 32 33)

dummyProgramId :: SolanaPublicKey
dummyProgramId = unsafeSolanaPublicKeyRaw (replicate 32 40)

keyK :: SolanaPublicKey
keyK = unsafeSolanaPublicKeyRaw (replicate 32 41)

fillerKey :: SolanaPublicKey
fillerKey = unsafeSolanaPublicKeyRaw (replicate 32 50)

lookupTable :: AddressLookupTableAccount
lookupTable = AddressLookupTableAccount tableKey [recipientPk, addr29, addr30]

tests :: TestTree
tests =
  testGroup
    "v0 message compilation (golden + behavior)"
    [ testCase "v0 message bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ixs =
              [ SP.transfer (fst payerKeys) recipientPk 1000000000,
                SP.transfer (fst payerKeys) addr29 500000
              ]
        case compileV0Message fixedBlockhash ixs [lookupTable] of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= requireFixture "v0-message" fs,
      testCase "v0 signed transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ixs =
              [ SP.transfer (fst payerKeys) recipientPk 1000000000,
                SP.transfer (fst payerKeys) addr29 500000
              ]
        case newV0TransactionIntent [snd payerKeys] ixs [lookupTable] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "v0-transfer-transaction" fs),
      testCase "table containing only the program id yields no lookups" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
            table = AddressLookupTableAccount tableKey [SP.systemProgramId]
        assertSameCompile
          (compileV0Message fixedBlockhash [ix] [table])
          (compileV0Message fixedBlockhash [ix] []),
      testCase "table containing the fee payer (a signer) yields no lookups" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
            table = AddressLookupTableAccount tableKey [fst payerKeys]
        assertSameCompile
          (compileV0Message fixedBlockhash [ix] [table])
          (compileV0Message fixedBlockhash [ix] []),
      testCase "table with no keys used by the instructions yields no lookups" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
            table = AddressLookupTableAccount tableKey [unrelatedKey]
        assertSameCompile
          (compileV0Message fixedBlockhash [ix] [table])
          (compileV0Message fixedBlockhash [ix] []),
      testCase "a key readonly in one instruction and writable in another drains as writable" $ do
        let payerPk = fst payerKeys
            ixReadonly =
              mkInstruction
                dummyProgramId
                [AccountMeta payerPk True True, AccountMeta keyK False False]
                ()
            ixWritable =
              mkInstruction
                dummyProgramId
                [AccountMeta payerPk True True, AccountMeta keyK False True]
                ()
            table = AddressLookupTableAccount tableKey [keyK]
        assertSameCompile
          (compileV0Message fixedBlockhash [ixReadonly, ixWritable] [table])
          (compileV0Message fixedBlockhash [ixWritable, ixWritable] [table]),
      testCase "the first table containing a key wins over a later one" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
            table1 = AddressLookupTableAccount tableKey [recipientPk]
            table2 = AddressLookupTableAccount otherTableKey [recipientPk]
        assertSameCompile
          (compileV0Message fixedBlockhash [ix] [table1, table2])
          (compileV0Message fixedBlockhash [ix] [table1]),
      testCase "a lookup index beyond byte range fails to compile" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
            -- 256 filler addresses (indexes 0..255) push recipientPk to
            -- index 256, one past what a Word8 lookup index can hold.
            oversizedTable = AddressLookupTableAccount tableKey (replicate 256 fillerKey <> [recipientPk])
        case compileV0Message fixedBlockhash [ix] [oversizedTable] of
          Left (MissingIndex msg) -> assertBool ("expected an \"overflow\" message, got: " <> msg) ("overflow" `isInfixOf` msg)
          Right _ -> assertFailure "expected an oversized lookup table to fail compilation"
    ]

-- | Compare two compile results by their message bytes ('CompileException'
-- has no 'Eq' instance to compare), failing loudly if either side failed.
assertSameCompile :: Either CompileException BS.ByteString -> Either CompileException BS.ByteString -> Assertion
assertSameCompile lhs rhs = case (lhs, rhs) of
  (Right l, Right r) -> l @?= r
  (Left err, _) -> assertFailure ("left side failed: " <> show err)
  (_, Left err) -> assertFailure ("right side failed: " <> show err)
