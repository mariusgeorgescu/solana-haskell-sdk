{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Message (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Block (BlockHash (..))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Message
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

fixedBlockhash :: BlockHash
fixedBlockhash = BlockHash (BS.replicate 32 9)

recipientPk :: SolanaPublicKey
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 2)

payerKeys :: (SolanaPublicKey, SolanaPrivateKey)
payerKeys =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just kp -> kp
    Nothing -> error "failed to derive payer keypair from seed"

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
          Right b64 -> b64 @?= toBase64String (requireFixture "transfer-transaction" fs)
    ]
