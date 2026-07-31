{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.SystemProgram (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

basePk, ownerPk, authorityPk, funderPk, newAccountPk :: SolanaPublicKey
basePk = unsafeSolanaPublicKeyRaw (replicate 32 3)
ownerPk = unsafeSolanaPublicKeyRaw (replicate 32 5)
authorityPk = unsafeSolanaPublicKeyRaw (replicate 32 6)
funderPk = unsafeSolanaPublicKeyRaw (replicate 32 7)
newAccountPk = unsafeSolanaPublicKeyRaw (replicate 32 8)

seedStr :: String
seedStr = "hello-seed"

dataOf :: Instruction -> BS.ByteString
dataOf = instrData . iData

tests :: TestTree
tests =
  withResource
    (loadFixtures "test/fixtures/system_instruction_data.json")
    (const (pure ()))
    $ \getFixtures ->
      testGroup
        "SystemProgram instruction data (golden)"
        [ testCase "CreateAccount" $ do
            fs <- getFixtures
            dataOf (SP.createAccount funderPk newAccountPk 1000000 165 ownerPk)
              @?= requireFixture "CreateAccount" fs,
          testCase "Assign" $ do
            fs <- getFixtures
            dataOf (SP.assignAccount newAccountPk ownerPk)
              @?= requireFixture "Assign" fs,
          testCase "Transfer" $ do
            fs <- getFixtures
            dataOf (SP.transfer funderPk newAccountPk 1000000)
              @?= requireFixture "Transfer" fs,
          testCase "CreateAccountWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.createAccountWithSeed basePk seedStr funderPk newAccountPk 1000000 165 ownerPk)
              @?= requireFixture "CreateAccountWithSeed" fs
        ]
