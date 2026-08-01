{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.Stake (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, unsafeSolanaPublicKeyRaw)
import Network.Solana.NativePrograms.Stake qualified as Stake
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

custodianPk, newAuthPk :: SolanaPublicKey
custodianPk = unsafeSolanaPublicKeyRaw (replicate 32 18)
newAuthPk = unsafeSolanaPublicKeyRaw (replicate 32 20)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

fixedAuthorized :: Stake.Authorized
fixedAuthorized = Stake.Authorized {Stake.aStaker = payerPk, Stake.aWithdrawer = payerPk}

fixedLockup :: Stake.Lockup
fixedLockup = Stake.Lockup {Stake.lUnixTimestamp = 1700000000, Stake.lEpoch = 300, Stake.lCustodian = custodianPk}

enc :: Stake.StakeInstruction -> BS.ByteString
enc = BL.toStrict . encode

genStakeInstruction :: Gen Stake.StakeInstruction
genStakeInstruction =
  oneof
    [ Stake.Initialize <$> genAuthorized <*> genLockup,
      Stake.Authorize <$> genPk <*> genAuth,
      pure Stake.DelegateStake,
      Stake.Split <$> arbitrary,
      Stake.Withdraw <$> arbitrary,
      pure Stake.Deactivate,
      Stake.SetLockup <$> genLockupArgs,
      pure Stake.Merge,
      pure Stake.InitializeChecked,
      Stake.AuthorizeChecked <$> genAuth,
      pure Stake.GetMinimumDelegation
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genAuth = elements [Stake.AuthorizeStaker, Stake.AuthorizeWithdrawer]
    genAuthorized = Stake.Authorized <$> genPk <*> genPk
    genLockup = Stake.Lockup <$> arbitrary <*> arbitrary <*> genPk
    genLockupArgs =
      Stake.LockupArgs
        <$> oneof [pure Nothing, Just <$> arbitrary]
        <*> oneof [pure Nothing, Just <$> arbitrary]
        <*> oneof [pure Nothing, Just <$> genPk]

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/stake_instruction_data.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Stake instruction data (golden + properties)"
      [ goldenCase getFixtures "Initialize" (Stake.Initialize fixedAuthorized fixedLockup),
        goldenCase getFixtures "Authorize-staker" (Stake.Authorize newAuthPk Stake.AuthorizeStaker),
        goldenCase getFixtures "Authorize-withdrawer" (Stake.Authorize newAuthPk Stake.AuthorizeWithdrawer),
        goldenCase getFixtures "DelegateStake" Stake.DelegateStake,
        goldenCase getFixtures "Split" (Stake.Split 250000),
        goldenCase getFixtures "Withdraw" (Stake.Withdraw 500000),
        goldenCase getFixtures "Deactivate" Stake.Deactivate,
        goldenCase getFixtures "SetLockup-some" (Stake.SetLockup (Stake.LockupArgs (Just 1700000000) (Just 300) (Just custodianPk))),
        goldenCase getFixtures "SetLockup-none" (Stake.SetLockup (Stake.LockupArgs Nothing Nothing Nothing)),
        goldenCase getFixtures "Merge" Stake.Merge,
        goldenCase getFixtures "InitializeChecked" Stake.InitializeChecked,
        goldenCase getFixtures "AuthorizeChecked" (Stake.AuthorizeChecked Stake.AuthorizeWithdrawer),
        goldenCase getFixtures "GetMinimumDelegation" Stake.GetMinimumDelegation,
        testProperty "Binary round-trip" $
          forAll genStakeInstruction $ \si -> decode (encode si) === si,
        testCase "decode fails on unknown discriminant" $
          case decodeOrFail (BL.pack [16, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Stake.StakeInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 16"
      ]
  where
    goldenCase getFixtures name si =
      testCase name $ do
        fs <- getFixtures
        enc si @?= requireFixture name fs
