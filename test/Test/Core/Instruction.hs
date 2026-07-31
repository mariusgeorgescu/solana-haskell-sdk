{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Instruction (tests) where

import Data.Aeson (eitherDecode)
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
        iProgramId ix @?= progPk,
      testCase "CompiledInstruction FromJSON rejects invalid base58 data" $
        case eitherDecode "{\"programIdIndex\":0,\"accounts\":[0,1],\"data\":\"0OIl\"}" :: Either String CompiledInstruction of
          Left _ -> pure ()
          Right _ -> assertFailure "expected parse failure on invalid base58"
    ]
