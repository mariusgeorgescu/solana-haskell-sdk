{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Instruction (tests) where

import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Test.Tasty
import Test.Tasty.HUnit

progPk, accPk :: SolanaPublicKey
progPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
accPk = unsafeSolanaPublicKeyRaw (replicate 32 12)

tests :: TestTree
tests =
  testGroup
    "instruction"
    [ testCase "mkInstruction keeps exactly the given account metas" $ do
        let meta = AccountMeta {accountPubKey = accPk, isSigner = True, isWritable = True}
            ix = mkInstruction progPk [meta] ()
        iAccounts ix @?= [meta]
        iProgramId ix @?= progPk
    ]
