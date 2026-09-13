{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Instruction (tests) where

import Data.Aeson (eitherDecode)
import Data.Binary (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Either (isRight)
import Data.List (isInfixOf)
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

progPk, accPk, fillerKey :: SolanaPublicKey
progPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
accPk = unsafeSolanaPublicKeyRaw (replicate 32 12)
fillerKey = unsafeSolanaPublicKeyRaw (replicate 32 50)

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
          Right _ -> assertFailure "expected parse failure on invalid base58",
      testCase "compileInstruction rejects an account at index 256 instead of wrapping it" $ do
        let keys = replicate 256 fillerKey <> [accPk]
            ix = mkInstruction fillerKey [AccountMeta accPk False False] ()
        case compileInstruction keys ix of
          Left (MissingIndex msg) -> assertBool ("expected \"overflow\" in: " <> msg) ("overflow" `isInfixOf` msg)
          Right _ -> assertFailure "compiled an account index of 256",
      testCase "compileInstruction rejects a program id at index 256" $ do
        let keys = replicate 256 fillerKey <> [progPk]
            ix = mkInstruction progPk [AccountMeta fillerKey False False] ()
        case compileInstruction keys ix of
          Left (MissingIndex msg) -> assertBool ("expected \"overflow\" in: " <> msg) ("overflow" `isInfixOf` msg)
          Right _ -> assertFailure "compiled a program id index of 256",
      testCase "compileInstruction keeps index 255 (boundary)" $ do
        let keys = replicate 255 fillerKey <> [accPk]
            ix = mkInstruction fillerKey [AccountMeta accPk False False] ()
        case compileInstruction keys ix of
          Left err -> assertFailure (show err)
          Right ci -> BL.unpack (encode ci) @?= [0, 1, 255, 0],
      testProperty "compileInstruction succeeds exactly when the key position fits in a byte" $
        forAll (chooseInt (0, 600)) $ \i ->
          let keys = replicate i fillerKey <> [accPk]
              ix = mkInstruction accPk [AccountMeta accPk False False] ()
           in isRight (compileInstruction keys ix) === (i <= 255)
    ]
