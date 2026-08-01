{-# LANGUAGE ScopedTypeVariables #-}

module Test.NativePrograms.Secp256k1 (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Instruction (iAccounts, iData, instrData)
import Network.Solana.NativePrograms.Secp256k1 qualified as Secp
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

genOffsets :: Gen Secp.SecpSignatureOffsets
genOffsets =
  Secp.SecpSignatureOffsets
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/secp256k1.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Secp256k1 instruction data (golden + properties)"
      [ testCase "newSecp256k1Instruction reconstructs the full instruction data" $ do
          fs <- getFixtures
          let ethAddress = requireFixture "secp-eth-address" fs
              signature = requireFixture "secp-signature" fs
              recoveryId = BS.head (requireFixture "secp-recovery-id" fs)
              message = requireFixture "secp-message" fs
              ix = Secp.newSecp256k1Instruction ethAddress signature recoveryId message
          instrData (iData ix) @?= requireFixture "secp-full-instruction-data" fs,
        testCase "offsets golden: decode from the full instruction data" $ do
          fs <- getFixtures
          let full = requireFixture "secp-full-instruction-data" fs
              offsets = decode (BL.fromStrict (BS.take 11 (BS.drop 1 full))) :: Secp.SecpSignatureOffsets
          offsets
            @?= Secp.SecpSignatureOffsets
              { Secp.ssoSignatureOffset = 32,
                Secp.ssoSignatureInstructionIndex = 0,
                Secp.ssoEthAddressOffset = 12,
                Secp.ssoEthAddressInstructionIndex = 0,
                Secp.ssoMessageDataOffset = 97,
                Secp.ssoMessageDataSize = 10,
                Secp.ssoMessageInstructionIndex = 0
              },
        testProperty "SecpSignatureOffsets Binary round-trip" $
          forAll genOffsets $ \o -> decode (encode o) === o,
        testCase "newSecp256k1Instruction rejects a 19-byte ethAddress" $ do
          result <-
            try
              ( evaluate
                  (Secp.newSecp256k1Instruction (BS.replicate 19 0) (BS.replicate 64 0) 0 (BS.replicate 10 0))
              )
          case result of
            Left (_ :: ErrorCall) -> pure ()
            Right _ -> assertFailure "expected error for a 19-byte ethAddress",
        testCase "newSecp256k1Instruction rejects a 63-byte signature" $ do
          result <-
            try
              ( evaluate
                  (Secp.newSecp256k1Instruction (BS.replicate 20 0) (BS.replicate 63 0) 0 (BS.replicate 10 0))
              )
          case result of
            Left (_ :: ErrorCall) -> pure ()
            Right _ -> assertFailure "expected error for a 63-byte signature",
        testCase "newSecp256k1Instruction takes no accounts" $
          iAccounts (Secp.newSecp256k1Instruction (BS.replicate 20 0) (BS.replicate 64 0) 0 (BS.replicate 10 0))
            @?= []
      ]
