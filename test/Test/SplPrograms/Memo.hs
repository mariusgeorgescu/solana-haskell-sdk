{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.Memo (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.SplPrograms.Memo qualified as Memo
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

signerPk :: SolanaPublicKey
signerPk = unsafeSolanaPublicKeyRaw (replicate 32 4)

dataOf :: Instruction -> BS.ByteString
dataOf = instrData . iData

tests :: TestTree
tests =
  testGroup
    "Memo"
    [ testCase "memo data bytes match Rust" $ do
        fs <- loadFixtures "test/fixtures/memo_instruction_data.json"
        dataOf (Memo.buildMemo "hello-memo" []) @?= requireFixture "memo-data" fs,
      testCase "signers become readonly-signer metas" $
        iAccounts (Memo.buildMemo "m" [signerPk])
          @?= [AccountMeta {accountPubKey = signerPk, isSigner = True, isWritable = False}],
      testCase "no signers means no account metas" $
        iAccounts (Memo.buildMemo "m" []) @?= []
    ]
