{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.ComputeBudget (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.ComputeBudget qualified as CB
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

dataOf :: Instruction -> BS.ByteString
dataOf = instrData . iData

genComputeBudgetInstruction :: Gen CB.ComputeBudgetInstruction
genComputeBudgetInstruction =
  oneof
    [ CB.RequestHeapFrame <$> arbitrary,
      CB.SetComputeUnitLimit <$> arbitrary,
      CB.SetComputeUnitPrice <$> arbitrary,
      CB.SetLoadedAccountsDataSizeLimit <$> arbitrary
    ]

tests :: TestTree
tests =
  withResource
    (loadFixtures "test/fixtures/compute_budget_instruction_data.json")
    (const (pure ()))
    $ \getFixtures ->
      testGroup
        "ComputeBudget (golden + properties)"
        [ testCase "RequestHeapFrame" $ do
            fs <- getFixtures
            dataOf (CB.requestHeapFrame 32768) @?= requireFixture "RequestHeapFrame" fs,
          testCase "SetComputeUnitLimit" $ do
            fs <- getFixtures
            dataOf (CB.setComputeUnitLimit 200000) @?= requireFixture "SetComputeUnitLimit" fs,
          testCase "SetComputeUnitPrice" $ do
            fs <- getFixtures
            dataOf (CB.setComputeUnitPrice 1000) @?= requireFixture "SetComputeUnitPrice" fs,
          testCase "SetLoadedAccountsDataSizeLimit" $ do
            fs <- getFixtures
            dataOf (CB.setLoadedAccountsDataSizeLimit 65536)
              @?= requireFixture "SetLoadedAccountsDataSizeLimit" fs,
          testCase "builders take no accounts" $ do
            iAccounts (CB.requestHeapFrame 1024) @?= []
            iAccounts (CB.setComputeUnitLimit 1) @?= []
            iAccounts (CB.setComputeUnitPrice 1) @?= []
            iAccounts (CB.setLoadedAccountsDataSizeLimit 1) @?= [],
          testProperty "Binary round-trip" $
            forAll genComputeBudgetInstruction $ \cbi -> decode (encode cbi) === cbi,
          testCase "decode fails on unknown discriminant" $
            case decodeOrFail (BL.pack [5]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, CB.ComputeBudgetInstruction) of
              Left _ -> pure ()
              Right _ -> assertFailure "expected decode failure for discriminant 5"
        ]
