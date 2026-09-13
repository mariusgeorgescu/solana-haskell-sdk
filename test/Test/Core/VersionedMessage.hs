{-# LANGUAGE OverloadedStrings #-}

module Test.Core.VersionedMessage (tests) where

import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction (AccountMeta (..), CompileException (..), mkInstruction)
import Network.Solana.Core.VersionedMessage
import Network.Solana.NativePrograms.AddressLookupTable qualified as ALT
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.SplPrograms.Memo qualified as Memo
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

newAccountKeys :: (SolanaPublicKey, SolanaPrivateKey)
newAccountKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 10) of
    Just kp -> kp
    Nothing -> error "failed to derive new-account keypair from seed"

extraKeys :: (SolanaPublicKey, SolanaPrivateKey)
extraKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 40) of
    Just kp -> kp
    Nothing -> error "failed to derive extra keypair from seed"

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

ownerPk :: SolanaPublicKey
ownerPk = unsafeSolanaPublicKeyRaw (replicate 32 5)

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
          Right _ -> assertFailure "expected an oversized lookup table to fail compilation",
      testCase "decoded lookup table state feeds v0 compilation end-to-end (bridge)" $ do
        stateFs <- loadFixtures "test/fixtures/state_fixtures.json"
        txFs <- loadFixtures "test/fixtures/transactions.json"
        case ALT.decodeLookupTable (requireFixture "lookup-table" stateFs) of
          Left err -> assertFailure ("decode failed: " <> err)
          Right lts -> do
            let table = ALT.lookupTableToAccount tableKey lts
                ixs =
                  [ SP.transfer (fst payerKeys) recipientPk 1000000000,
                    SP.transfer (fst payerKeys) addr29 500000
                  ]
            case compileV0Message fixedBlockhash ixs [table] of
              Left err -> assertFailure (show err)
              Right bytes -> bytes @?= requireFixture "v0-message" txFs,
      testCase "v0 signed transaction via explicit fee payer matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ixs =
              [ SP.transfer (fst payerKeys) recipientPk 1000000000,
                SP.transfer (fst payerKeys) addr29 500000
              ]
        case newV0TransactionIntentWithPayer (fst payerKeys) [snd payerKeys] ixs [lookupTable] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "v0-transfer-transaction" fs),
      testCase "v0 memo-only transaction via explicit fee payer matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = Memo.buildMemo "hello-memo" [fst payerKeys]
        case newV0TransactionIntentWithPayer (fst payerKeys) [snd payerKeys] [ix] [] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "v0-memo-only-transaction" fs),
      testCase "v0 memo-only message: explicit fee payer seeds a writable signer, compileV0Message does not" $ do
        let ix = Memo.buildMemo "hello-memo" [fst payerKeys]
        case compileV0MessageWithPayer (fst payerKeys) fixedBlockhash [ix] [] of
          Left err -> assertFailure (show err)
          Right bytes -> BS.take 4 bytes @?= BS.pack [0x80, 1, 0, 1]
        case compileV0Message fixedBlockhash [ix] [] of
          Left err -> assertFailure (show err)
          Right bytes -> BS.take 4 bytes @?= BS.pack [0x80, 1, 1, 1],
      testCase "v0 explicit fee payer: signers may be given in any order" $ do
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
        case ( newV0TransactionIntentWithPayer (fst payerKeys) [snd newAccountKeys, snd payerKeys] [ix] [] fixedBlockhash,
               newV0TransactionIntentWithPayer (fst payerKeys) [snd payerKeys, snd newAccountKeys] [ix] [] fixedBlockhash
             ) of
          (Right permuted, Right inOrder) -> permuted @?= inOrder
          (Left err, _) -> assertFailure (show err)
          (_, Left err) -> assertFailure (show err),
      testCase "v0 explicit fee payer: missing and unused signing keys are reported" $ do
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
        case newV0TransactionIntentWithPayer (fst payerKeys) [snd payerKeys] [ix] [] fixedBlockhash of
          Right _ -> assertFailure "expected Left for missing signer"
          Left (MissingIndex msg) -> assertBool ("expected \"missing signer\" in: " <> msg) ("missing signer" `isInfixOf` msg)
        case newV0TransactionIntentWithPayer (fst payerKeys) [snd payerKeys, snd newAccountKeys, snd extraKeys] [ix] [] fixedBlockhash of
          Right _ -> assertFailure "expected Left for unused signing key"
          Left (MissingIndex msg) -> assertBool ("expected \"unused signing key\" in: " <> msg) ("unused signing key" `isInfixOf` msg),
      testCase "static and table-loaded keys totalling more than 256 fail to compile" $ do
        -- Each table stays under 256 addresses (the per-table check passes);
        -- 2 static keys + 300 loaded keys exceed what a byte index can address.
        let tableA = AddressLookupTableAccount tableKey [numberedKey b | b <- [0 .. 199]]
            tableB = AddressLookupTableAccount otherTableKey [numberedKey b | b <- [200 .. 299]]
            ix = mkInstruction dummyProgramId (AccountMeta (fst payerKeys) True True : [AccountMeta (numberedKey b) False True | b <- [0 .. 299]]) ()
        case compileV0Message fixedBlockhash [ix] [tableA, tableB] of
          Left (MissingIndex msg) -> assertBool ("expected \"account index overflow\" in: " <> msg) ("account index overflow" `isInfixOf` msg)
          Right _ -> assertFailure "compiled a v0 message resolving more than 256 keys",
      testCase "static plus table-loaded keys totalling exactly 256 compile" $ do
        let table = AddressLookupTableAccount tableKey [numberedKey b | b <- [0 .. 253]]
            ix = mkInstruction dummyProgramId (AccountMeta (fst payerKeys) True True : [AccountMeta (numberedKey b) False True | b <- [0 .. 253]]) ()
        case compileV0Message fixedBlockhash [ix] [table] of
          Left err -> assertFailure (show err)
          Right _ -> pure ()
    ]

-- | A distinct 32-byte key per number (up to 65535), for large key sets.
numberedKey :: Int -> SolanaPublicKey
numberedKey b = unsafeSolanaPublicKeyRaw (2 : fromIntegral (b `div` 256) : fromIntegral (b `mod` 256) : replicate 29 0)

-- | Compare two compile results by their message bytes ('CompileException'
-- has no 'Eq' instance to compare), failing loudly if either side failed.
assertSameCompile :: Either CompileException BS.ByteString -> Either CompileException BS.ByteString -> Assertion
assertSameCompile lhs rhs = case (lhs, rhs) of
  (Right l, Right r) -> l @?= r
  (Left err, _) -> assertFailure ("left side failed: " <> show err)
  (_, Left err) -> assertFailure ("right side failed: " <> show err)
