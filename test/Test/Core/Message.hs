{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Message (tests) where

import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message
import Network.Solana.NativePrograms.ComputeBudget qualified as CB
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.SplPrograms.Memo qualified as Memo
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

ownerPk :: SolanaPublicKey
ownerPk = unsafeSolanaPublicKeyRaw (replicate 32 5)

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

keyPool :: [SolanaPublicKey]
keyPool = [unsafeSolanaPublicKeyRaw (replicate 32 b) | b <- [10 .. 15]]

genInstruction :: Gen Instruction
genInstruction = do
  progId <- elements keyPool
  n <- chooseInt (1, 4)
  metas <- vectorOf n genMeta
  pure (mkInstruction progId metas ())
  where
    genMeta = do
      k <- elements keyPool
      s <- arbitrary
      w <- arbitrary
      pure AccountMeta {accountPubKey = k, isSigner = s, isWritable = w}

expectedPrivileges :: [Instruction] -> Map.Map SolanaPublicKey (Bool, Bool)
expectedPrivileges instrs = Map.fromListWith merge (concatMap entries instrs)
  where
    merge (s1, w1) (s2, w2) = (s1 || s2, w1 || w2)
    entries i =
      (iProgramId i, (False, False))
        : [(accountPubKey m, (isSigner m, isWritable m)) | m <- iAccounts i]

prop_messageInvariants :: Property
prop_messageInvariants =
  forAll (resize 5 (listOf1 genInstruction)) $ \instrs ->
    let msg = mkNewMessage fixedBlockhash instrs
        keys = mAccountKeys msg
        privs = expectedPrivileges instrs
        hdr = mHeader msg
        nrs = fromIntegral (numRequiredSignatures hdr)
        nros = fromIntegral (numReadonlySignedAccounts hdr)
        nrou = fromIntegral (numReadonlyUnsignedAccounts hdr)
        (signed, unsigned) = splitAt nrs keys
        (sw, sr) = splitAt (nrs - nros) signed
        (uw, ur) = splitAt (length unsigned - nrou) unsigned
        signerOf k = maybe False fst (Map.lookup k privs)
        writableOf k = maybe False snd (Map.lookup k privs)
     in conjoin
          [ counterexample "duplicate keys" (keys === nub keys),
            counterexample "key set mismatch" (Map.keysSet privs === Set.fromList keys),
            counterexample "non-signer in signed section" (property (all signerOf signed)),
            counterexample "signer in unsigned section" (property (all (not . signerOf) unsigned)),
            counterexample "readonly key in writable section" (property (all writableOf (sw <> uw))),
            counterexample "writable key in readonly section" (property (all (not . writableOf) (sr <> ur)))
          ]

tests :: TestTree
tests =
  testGroup
    "message and transaction (golden)"
    [ testCase "payer pubkey from seed matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        getSolanaPublicKeyRaw (fst payerKeys) @?= requireFixture "payer-pubkey" fs,
      testCase "single transfer message bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/messages.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newMessage fixedBlockhash [ix] of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= requireFixture "transfer-message" fs,
      testCase "signed transfer transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntent [snd payerKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "transfer-transaction" fs),
      testCase "transfer + allocate message bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/messages.json"
        let transferIx = SP.transfer (fst payerKeys) recipientPk 1000000000
            allocateIx = SP.allocate (fst payerKeys) 200
        case newMessage fixedBlockhash [transferIx, allocateIx] of
          Left err -> assertFailure (show err)
          Right bytes -> bytes @?= requireFixture "transfer-allocate-message" fs,
      testCase "priority-fee transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ixs =
              [ CB.setComputeUnitLimit 200000,
                CB.setComputeUnitPrice 1000,
                SP.transfer (fst payerKeys) recipientPk 1000000000,
                Memo.buildMemo "hello-memo" [fst payerKeys]
              ]
        case newTransactionIntent [snd payerKeys] ixs fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "priority-transfer-transaction" fs),
      testCase "new-account pubkey from seed matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        getSolanaPublicKeyRaw (fst newAccountKeys) @?= requireFixture "new-account-pubkey" fs,
      testCase "two-signer create-account transaction matches Rust (payer pinned, not sorted)" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
        case newTransactionIntent [snd payerKeys, snd newAccountKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "two-signer-create-transaction" fs),
      testProperty "message header/key-ordering invariants" prop_messageInvariants
    ]
