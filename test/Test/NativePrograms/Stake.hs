{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.Stake (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.NativePrograms.Stake qualified as Stake
import Network.Solana.Sysvar qualified as Sysvar
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

custodianPk, newAuthPk, withdrawerPk :: SolanaPublicKey
custodianPk = unsafeSolanaPublicKeyRaw (replicate 32 18)
newAuthPk = unsafeSolanaPublicKeyRaw (replicate 32 20)
withdrawerPk = unsafeSolanaPublicKeyRaw (replicate 32 28)

stakeAcctPk, votePk, recipientPk' :: SolanaPublicKey
stakeAcctPk = unsafeSolanaPublicKeyRaw (replicate 32 21)
votePk = unsafeSolanaPublicKeyRaw (replicate 32 17)
recipientPk' = unsafeSolanaPublicKeyRaw (replicate 32 22)

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
tests = testGroup "Stake" [instructionDataTests, stakeStateTests]

stakeStateTests :: TestTree
stakeStateTests =
  withResource (loadFixtures "test/fixtures/state_fixtures.json") (const (pure ())) $ \getStateFixtures ->
    testGroup
      "Stake account state (golden + rejection)"
      [ testCase "decodeStakeAccount: stake-account golden (StakeActive)" $ do
          fs <- getStateFixtures
          let bs = requireFixture "stake-account" fs
          case Stake.decodeStakeAccount bs of
            Left err -> assertFailure $ "decode failed: " <> err
            Right (Stake.StakeActive meta delegation credits flags) -> do
              Stake.smRentExemptReserve meta @?= 2282880
              Stake.smAuthorized meta @?= fixedAuthorized
              Stake.smLockup meta @?= fixedLockup
              Stake.sdVoter delegation @?= votePk
              Stake.sdStake delegation @?= 1000000
              Stake.sdActivationEpoch delegation @?= 250
              Stake.sdDeactivationEpoch delegation @?= maxBound
              Stake.sdWarmupCooldownRate delegation @?= 0.25
              credits @?= 42
              flags @?= 0
            Right other -> assertFailure ("expected StakeActive, got " <> show other),
        testCase "decodeStakeAccount: stake-account-initialized golden (StakeInitialized)" $ do
          fs <- getStateFixtures
          let bs = requireFixture "stake-account-initialized" fs
          case Stake.decodeStakeAccount bs of
            Left err -> assertFailure $ "decode failed: " <> err
            Right (Stake.StakeInitialized meta) -> do
              Stake.smRentExemptReserve meta @?= 2282880
              Stake.smAuthorized meta @?= fixedAuthorized
              Stake.smLockup meta @?= fixedLockup
            Right other -> assertFailure ("expected StakeInitialized, got " <> show other),
        testCase "decodeStakeAccount: rejects discriminant 4" $ do
          let bs = BS.pack ([4, 0, 0, 0] <> replicate 196 0)
          case Stake.decodeStakeAccount bs of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 4",
        testCase "decodeStakeAccount: decode tolerates trailing padding" $ do
          fs <- getStateFixtures
          let bs = requireFixture "stake-account" fs
              padded = bs <> BS.replicate 3 0
          Stake.decodeStakeAccount padded @?= Stake.decodeStakeAccount bs
      ]

instructionDataTests :: TestTree
instructionDataTests =
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
            Right _ -> assertFailure "expected decode failure for discriminant 16",
        testCase "initialize metas" $
          iAccounts (Stake.initialize stakeAcctPk fixedAuthorized fixedLockup)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.rent False False
                ],
        testCase "authorize metas (no custodian)" $
          iAccounts (Stake.authorize stakeAcctPk payerPk newAuthPk Stake.AuthorizeStaker Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False
                ],
        testCase "authorize metas (custodian)" $
          iAccounts (Stake.authorize stakeAcctPk payerPk newAuthPk Stake.AuthorizeWithdrawer (Just custodianPk))
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta custodianPk True False
                ],
        testCase "delegateStake metas" $
          iAccounts (Stake.delegateStake stakeAcctPk payerPk votePk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta votePk False False,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta Stake.stakeConfigId False False,
                  AccountMeta payerPk True False
                ],
        testCase "split metas" $
          iAccounts (Stake.split stakeAcctPk recipientPk' payerPk 250000)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta payerPk True False
                ],
        testCase "withdraw metas (no custodian)" $
          iAccounts (Stake.withdraw stakeAcctPk recipientPk' payerPk 500000 Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta payerPk True False
                ],
        testCase "withdraw metas (custodian)" $
          iAccounts (Stake.withdraw stakeAcctPk recipientPk' payerPk 500000 (Just custodianPk))
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta payerPk True False,
                  AccountMeta custodianPk True False
                ],
        testCase "deactivate metas" $
          iAccounts (Stake.deactivate stakeAcctPk payerPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False
                ],
        testCase "setLockup metas" $
          iAccounts (Stake.setLockup stakeAcctPk (Stake.LockupArgs Nothing Nothing Nothing) custodianPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta custodianPk True False
                ],
        testCase "merge metas" $
          iAccounts (Stake.merge stakeAcctPk recipientPk' payerPk)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta Sysvar.stakeHistory False False,
                  AccountMeta payerPk True False
                ],
        testCase "initializeChecked metas" $
          iAccounts (Stake.initializeChecked stakeAcctPk (Stake.Authorized {Stake.aStaker = payerPk, Stake.aWithdrawer = withdrawerPk}))
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.rent False False,
                  AccountMeta payerPk False False,
                  AccountMeta withdrawerPk True False
                ],
        testCase "authorizeChecked metas" $
          iAccounts (Stake.authorizeChecked stakeAcctPk payerPk newAuthPk Stake.AuthorizeStaker Nothing)
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta newAuthPk True False
                ],
        testCase "authorizeChecked metas (custodian)" $
          iAccounts (Stake.authorizeChecked stakeAcctPk payerPk newAuthPk Stake.AuthorizeWithdrawer (Just custodianPk))
            @?= [ AccountMeta stakeAcctPk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta newAuthPk True False,
                  AccountMeta custodianPk True False
                ]
      ]
  where
    goldenCase getFixtures name si =
      testCase name $ do
        fs <- getFixtures
        enc si @?= requireFixture name fs
