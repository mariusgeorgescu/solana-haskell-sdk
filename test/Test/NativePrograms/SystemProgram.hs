{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.SystemProgram (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.Sysvar qualified as Sysvar
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
              @?= requireFixture "CreateAccountWithSeed" fs,
          testCase "AdvanceNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.advanceNonceAccount newAccountPk authorityPk)
              @?= requireFixture "AdvanceNonceAccount" fs,
          testCase "WithdrawNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.withdrawNonceAccount newAccountPk authorityPk funderPk 1000000)
              @?= requireFixture "WithdrawNonceAccount" fs,
          testCase "InitializeNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.initializeNonceAccount newAccountPk authorityPk)
              @?= requireFixture "InitializeNonceAccount" fs,
          testCase "AuthorizeNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.authorizeNonceAccount newAccountPk basePk authorityPk)
              @?= requireFixture "AuthorizeNonceAccount" fs,
          testCase "UpgradeNonceAccount" $ do
            fs <- getFixtures
            dataOf (SP.upgradeNonceAccount newAccountPk)
              @?= requireFixture "UpgradeNonceAccount" fs,
          testCase "advanceNonceAccount account metas" $
            iAccounts (SP.advanceNonceAccount newAccountPk authorityPk)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = authorityPk, isSigner = True, isWritable = False}
                  ],
          testCase "withdrawNonceAccount account metas" $
            iAccounts (SP.withdrawNonceAccount newAccountPk authorityPk funderPk 1000000)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = funderPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = Sysvar.recentBlockhashes, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
                    AccountMeta {accountPubKey = authorityPk, isSigner = True, isWritable = False}
                  ]
        ]
