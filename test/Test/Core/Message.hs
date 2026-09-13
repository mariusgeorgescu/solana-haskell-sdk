{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Message (tests) where

import Data.ByteString qualified as BS
import Data.List (isInfixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message
import Network.Solana.NativePrograms.ComputeBudget qualified as CB
import Network.Solana.NativePrograms.Stake qualified as Stake
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata
import Network.Solana.SplPrograms.Memo qualified as Memo
import Network.Solana.SplPrograms.Token qualified as Tok
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

votePk' :: SolanaPublicKey
votePk' = unsafeSolanaPublicKeyRaw (replicate 32 17)

custodianPk' :: SolanaPublicKey
custodianPk' = unsafeSolanaPublicKeyRaw (replicate 32 18)

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
            counterexample "signer in unsigned section" (property (not (any signerOf unsigned))),
            counterexample "readonly key in writable section" (property (all writableOf (sw <> uw))),
            counterexample "writable key in readonly section" (property (not (any writableOf (sr <> ur))))
          ]

-- | 'mkNewMessageWithPayer' must pin the named payer to account 0 as a
-- writable signer whatever the instructions declare (or fail to declare)
-- about it.
prop_payerInvariants :: Property
prop_payerInvariants =
  forAll (resize 5 (listOf1 genInstruction)) $ \instrs ->
    forAll (elements keyPool) $ \payer ->
      let msg = mkNewMessageWithPayer payer fixedBlockhash instrs
          hdr = mHeader msg
          keys = mAccountKeys msg
       in conjoin
            [ counterexample "payer is not account 0" (take 1 keys === [payer]),
              counterexample
                "no writable signer"
                (property (numReadonlySignedAccounts hdr < numRequiredSignatures hdr)),
              counterexample "duplicate keys" (keys === nub keys)
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
      testCase "ATA create + transferChecked transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)
            walletPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
            payerPk = fst payerKeys
            srcAta = fromMaybe (error "no src ata") (Ata.getAssociatedTokenAddress payerPk mintPk)
            dstAta = fromMaybe (error "no dst ata") (Ata.getAssociatedTokenAddress walletPk mintPk)
            ixs =
              [ Ata.createAssociatedTokenAccountIdempotent payerPk walletPk mintPk,
                Tok.transferChecked srcAta mintPk dstAta payerPk [] 1000000 6
              ]
        case newTransactionIntent [snd payerKeys] ixs fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "ata-transfer-transaction" fs),
      testCase "stake setup transaction matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let stakePk = fst newAccountKeys
            authorized = Stake.Authorized {Stake.aStaker = fst payerKeys, Stake.aWithdrawer = fst payerKeys}
            lockup = Stake.Lockup {Stake.lUnixTimestamp = 1700000000, Stake.lEpoch = 300, Stake.lCustodian = custodianPk'}
            ixs =
              [ SP.createAccount (fst payerKeys) stakePk 1000000 200 Stake.stakeProgramId,
                Stake.initialize stakePk authorized lockup,
                Stake.delegateStake stakePk (fst payerKeys) votePk'
              ]
        case newTransactionIntent [snd payerKeys, snd newAccountKeys] ixs fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "stake-setup-transaction" fs),
      testCase "sponsored transfer via explicit fee payer matches Rust (signers in wrong order)" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntentWithPayer (fst newAccountKeys) [snd payerKeys, snd newAccountKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "sponsored-transfer-transaction" fs),
      testCase "sponsored transfer via explicit fee payer matches Rust (signers in message order)" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntentWithPayer (fst newAccountKeys) [snd newAccountKeys, snd payerKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "sponsored-transfer-transaction" fs),
      testCase "two-signer create-account via explicit fee payer matches Rust (signers permuted)" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
        case newTransactionIntentWithPayer (fst payerKeys) [snd newAccountKeys, snd payerKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "two-signer-create-transaction" fs),
      testCase "explicit fee payer: missing signer key is reported" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntentWithPayer (fst newAccountKeys) [snd payerKeys] [ix] fixedBlockhash of
          Right _ -> assertFailure "expected Left for missing signer"
          Left (MissingIndex msg) -> assertBool ("expected \"missing signer\" in: " <> msg) ("missing signer" `isInfixOf` msg),
      testCase "explicit fee payer: unused signing key is reported" $ do
        let ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newTransactionIntentWithPayer (fst newAccountKeys) [snd payerKeys, snd newAccountKeys, snd extraKeys] [ix] fixedBlockhash of
          Right _ -> assertFailure "expected Left for unused signing key"
          Left (MissingIndex msg) -> assertBool ("expected \"unused signing key\" in: " <> msg) ("unused signing key" `isInfixOf` msg),
      testCase "durable-nonce transfer transaction matches Rust" $ do
        txFs <- loadFixtures "test/fixtures/transactions.json"
        stateFs <- loadFixtures "test/fixtures/state_fixtures.json"
        let noncePk = unsafeSolanaPublicKeyRaw (replicate 32 37)
            durableNonceBh = BlockHash (requireFixture "nonce-durable-hash" stateFs)
            ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newDurableNonceTransactionIntent [snd payerKeys] noncePk (fst payerKeys) [ix] durableNonceBh of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "nonce-transfer-transaction" txFs),
      testCase "durable-nonce transfer via explicit fee payer matches Rust (payer == authority)" $ do
        txFs <- loadFixtures "test/fixtures/transactions.json"
        stateFs <- loadFixtures "test/fixtures/state_fixtures.json"
        let noncePk = unsafeSolanaPublicKeyRaw (replicate 32 37)
            durableNonceBh = BlockHash (requireFixture "nonce-durable-hash" stateFs)
            ix = SP.transfer (fst payerKeys) recipientPk 1000000000
        case newDurableNonceTransactionIntentWithPayer (fst payerKeys) [snd payerKeys] noncePk (fst payerKeys) [ix] durableNonceBh of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "nonce-transfer-transaction" txFs),
      testCase "durable-nonce-only message via explicit fee payer seeds payer as writable signer" $ do
        stateFs <- loadFixtures "test/fixtures/state_fixtures.json"
        let noncePk = unsafeSolanaPublicKeyRaw (replicate 32 37)
            durableNonceBh = BlockHash (requireFixture "nonce-durable-hash" stateFs)
            sponsorPk = fst newAccountKeys
            sponsorPriv = snd newAccountKeys
            authorityPk = fst payerKeys
            authorityPriv = snd payerKeys
        case newDurableNonceTransactionIntentWithPayer sponsorPk [sponsorPriv, authorityPriv] noncePk authorityPk [] durableNonceBh of
          Left err -> assertFailure (show err)
          Right b64 -> BS.head (fromBase64String b64) @?= 2,
      testProperty "message header/key-ordering invariants" prop_messageInvariants,
      testCase "memo-only transaction via explicit fee payer matches Rust" $ do
        fs <- loadFixtures "test/fixtures/transactions.json"
        let ix = Memo.buildMemo "hello-memo" [fst payerKeys]
        case newTransactionIntentWithPayer (fst payerKeys) [snd payerKeys] [ix] fixedBlockhash of
          Left err -> assertFailure (show err)
          Right b64 -> b64 @?= toBase64String (requireFixture "memo-only-transaction" fs),
      testCase "memo-only message: explicit fee payer seeds a writable signer, instruction-derived payer does not" $ do
        let ix = Memo.buildMemo "hello-memo" [fst payerKeys]
        mHeader (mkNewMessageWithPayer (fst payerKeys) fixedBlockhash [ix]) @?= MessageHeader 1 0 1
        mHeader (mkNewMessage fixedBlockhash [ix]) @?= MessageHeader 1 1 1,
      testProperty "explicit fee payer is account 0 and a writable signer" prop_payerInvariants,
      testCase "orderSigningKeys: permuted keys are ordered to the message's required signers" $ do
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
            msg = mkNewMessageWithPayer (fst newAccountKeys) fixedBlockhash [ix]
        case orderSigningKeys msg [snd payerKeys, snd newAccountKeys] of
          Left err -> assertFailure (show err)
          Right ordered -> map toSolanaPublicKey ordered @?= take 2 (mAccountKeys msg),
      testCase "orderSigningKeys: missing and unused keys are reported" $ do
        let ix = SP.createAccount (fst payerKeys) (fst newAccountKeys) 1000000 165 ownerPk
            msg = mkNewMessageWithPayer (fst newAccountKeys) fixedBlockhash [ix]
        case orderSigningKeys msg [snd payerKeys] of
          Right _ -> assertFailure "expected Left for missing signer"
          Left (MissingIndex msg') ->
            assertBool ("expected \"missing signer\" in: " <> msg') ("missing signer" `isInfixOf` msg')
        case orderSigningKeys msg [snd payerKeys, snd newAccountKeys, snd extraKeys] of
          Right _ -> assertFailure "expected Left for unused signing key"
          Left (MissingIndex msg') ->
            assertBool ("expected \"unused signing key\" in: " <> msg') ("unused signing key" `isInfixOf` msg'),
      testCase "a message with more than 256 account keys fails to compile instead of wrapping indices" $ do
        let metas = [AccountMeta (unsafeSolanaPublicKeyRaw (replicate 31 7 <> [b])) False False | b <- [0 .. 254]]
            ix = mkInstruction Memo.memoProgramId (AccountMeta (fst payerKeys) True True : metas) ()
        case newMessage fixedBlockhash [ix] of
          Left (MissingIndex msg) -> assertBool ("expected \"overflow\" in: " <> msg) ("overflow" `isInfixOf` msg)
          Right _ -> assertFailure "compiled a message with 257 account keys"
        case newTransactionIntentWithPayer (fst payerKeys) [snd payerKeys] [ix] fixedBlockhash of
          Left (MissingIndex msg) -> assertBool ("expected \"overflow\" in: " <> msg) ("overflow" `isInfixOf` msg)
          Right _ -> assertFailure "signed a message with 257 account keys",
      testCase "a message with exactly 256 account keys compiles and its header does not wrap" $ do
        let metas = [AccountMeta (unsafeSolanaPublicKeyRaw (replicate 31 7 <> [b])) False False | b <- [0 .. 253]]
            ix = mkInstruction Memo.memoProgramId (AccountMeta (fst payerKeys) True True : metas) ()
        numReadonlyUnsignedAccounts (mHeader (mkNewMessage fixedBlockhash [ix])) @?= 255
        case newMessage fixedBlockhash [ix] of
          Left err -> assertFailure (show err)
          Right _ -> pure ()
    ]
