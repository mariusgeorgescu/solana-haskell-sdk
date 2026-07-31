{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.SystemProgram (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.Sysvar qualified as Sysvar
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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

genSystemInstruction :: Gen SP.SystemInstruction
genSystemInstruction =
  oneof
    [ SP.CreateAccount <$> arbitrary <*> arbitrary <*> genPk,
      SP.Assign <$> genPk,
      SP.Transfer <$> arbitrary,
      SP.CreateAccountWithSeed <$> genPk <*> genSeed <*> arbitrary <*> arbitrary <*> genPk,
      pure SP.AdvanceNonceAccount,
      SP.WithdrawNonceAccount <$> arbitrary,
      SP.InitializeNonceAccount <$> genPk,
      SP.AuthorizeNonceAccount <$> genPk,
      SP.Allocate <$> arbitrary,
      SP.AllocateWithSeed <$> genPk <*> genSeed <*> arbitrary <*> genPk,
      SP.AssignWithSeed <$> genPk <*> genSeed <*> genPk,
      SP.TransferWithSeed <$> arbitrary <*> genSeed <*> genPk,
      pure SP.UpgradeNonceAccount
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genSeed = listOf (elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))

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
          testCase "Allocate" $ do
            fs <- getFixtures
            dataOf (SP.allocate newAccountPk 165)
              @?= requireFixture "Allocate" fs,
          testCase "AllocateWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.allocateWithSeed newAccountPk basePk seedStr 165 ownerPk)
              @?= requireFixture "AllocateWithSeed" fs,
          testCase "AssignWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.assignWithSeed newAccountPk basePk seedStr ownerPk)
              @?= requireFixture "AssignWithSeed" fs,
          testCase "TransferWithSeed" $ do
            fs <- getFixtures
            dataOf (SP.transferWithSeed funderPk basePk seedStr ownerPk newAccountPk 1000000)
              @?= requireFixture "TransferWithSeed" fs,
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
                  ],
          testCase "allocateWithSeed account metas" $
            iAccounts (SP.allocateWithSeed newAccountPk basePk seedStr 165 ownerPk)
              @?= [ AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = basePk, isSigner = True, isWritable = False}
                  ],
          testCase "transferWithSeed account metas" $
            iAccounts (SP.transferWithSeed funderPk basePk seedStr ownerPk newAccountPk 1000000)
              @?= [ AccountMeta {accountPubKey = funderPk, isSigner = False, isWritable = True},
                    AccountMeta {accountPubKey = basePk, isSigner = True, isWritable = False},
                    AccountMeta {accountPubKey = newAccountPk, isSigner = False, isWritable = True}
                  ],
          testProperty "SystemInstruction Binary round-trip" $
            forAll genSystemInstruction $ \si -> decode (encode si) === si,
          testCase "decode fails on unknown discriminant" $
            case decodeOrFail (BL.pack [13, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, SP.SystemInstruction) of
              Left _ -> pure ()
              Right _ -> assertFailure "expected decode failure for discriminant 13"
        ]
