{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.Vote (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, getSolanaPublicKeyRaw, toBase58String, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.NativePrograms.Vote qualified as Vote
import Network.Solana.Sysvar qualified as Sysvar
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

votePk, nodePk, newAuthPk, recipientPk' :: SolanaPublicKey
votePk = unsafeSolanaPublicKeyRaw (replicate 32 17)
nodePk = unsafeSolanaPublicKeyRaw (replicate 32 19)
newAuthPk = unsafeSolanaPublicKeyRaw (replicate 32 20)
recipientPk' = unsafeSolanaPublicKeyRaw (replicate 32 22)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

fixedVoteInit :: Vote.VoteInit
fixedVoteInit =
  Vote.VoteInit
    { Vote.viNodePubkey = nodePk,
      Vote.viAuthorizedVoter = payerPk,
      Vote.viAuthorizedWithdrawer = payerPk,
      Vote.viCommission = 5
    }

enc :: Vote.VoteInstruction -> BS.ByteString
enc = BL.toStrict . encode

genVoteInstruction :: Gen Vote.VoteInstruction
genVoteInstruction =
  oneof
    [ Vote.InitializeAccount <$> genVoteInit,
      Vote.Authorize <$> genPk <*> genAuth,
      Vote.Withdraw <$> arbitrary,
      pure Vote.UpdateValidatorIdentity,
      Vote.UpdateCommission <$> arbitrary,
      Vote.AuthorizeChecked <$> genAuth
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genAuth = elements [Vote.AuthorizeVoter, Vote.AuthorizeWithdrawer]
    genVoteInit = Vote.VoteInit <$> genPk <*> genPk <*> genPk <*> arbitrary

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/vote_instruction_data.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Vote instruction data (golden + properties)"
      [ goldenCase getFixtures "InitializeAccount" (Vote.InitializeAccount fixedVoteInit),
        goldenCase getFixtures "Authorize-voter" (Vote.Authorize newAuthPk Vote.AuthorizeVoter),
        goldenCase getFixtures "Authorize-withdrawer" (Vote.Authorize newAuthPk Vote.AuthorizeWithdrawer),
        goldenCase getFixtures "Withdraw" (Vote.Withdraw 500000),
        goldenCase getFixtures "UpdateValidatorIdentity" Vote.UpdateValidatorIdentity,
        goldenCase getFixtures "UpdateCommission" (Vote.UpdateCommission 5),
        goldenCase getFixtures "AuthorizeChecked" (Vote.AuthorizeChecked Vote.AuthorizeWithdrawer),
        testProperty "Binary round-trip" $
          forAll genVoteInstruction $ \vi -> decode (encode vi) === vi,
        testCase "decode fails on unknown discriminant" $
          case decodeOrFail (BL.pack [2, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Vote.VoteInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 2",
        testCase "initializeVoteAccount metas" $
          iAccounts (Vote.initializeVoteAccount votePk fixedVoteInit)
            @?= [ AccountMeta votePk False True,
                  AccountMeta Sysvar.rent False False,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta nodePk True False
                ],
        testCase "authorizeVote metas" $
          iAccounts (Vote.authorizeVote votePk payerPk newAuthPk Vote.AuthorizeVoter)
            @?= [ AccountMeta votePk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False
                ],
        testCase "withdrawVote metas" $
          iAccounts (Vote.withdrawVote votePk recipientPk' payerPk 500000)
            @?= [ AccountMeta votePk False True,
                  AccountMeta recipientPk' False True,
                  AccountMeta payerPk True False
                ],
        testCase "updateValidatorIdentity metas" $
          iAccounts (Vote.updateValidatorIdentity votePk nodePk payerPk)
            @?= [ AccountMeta votePk False True,
                  AccountMeta nodePk True False,
                  AccountMeta payerPk True False
                ],
        testCase "updateCommission metas" $
          iAccounts (Vote.updateCommission votePk payerPk 5)
            @?= [ AccountMeta votePk False True,
                  AccountMeta payerPk True False
                ],
        testCase "authorizeVoteChecked metas" $
          iAccounts (Vote.authorizeVoteChecked votePk payerPk newAuthPk Vote.AuthorizeWithdrawer)
            @?= [ AccountMeta votePk False True,
                  AccountMeta Sysvar.clock False False,
                  AccountMeta payerPk True False,
                  AccountMeta newAuthPk True False
                ],
        testCase "voteProgramId decodes to canonical bytes" $ do
          toBase58String (getSolanaPublicKeyRaw Vote.voteProgramId) @?= "Vote111111111111111111111111111111111111111"
          BS.length (getSolanaPublicKeyRaw Vote.voteProgramId) @?= 32
      ]
  where
    goldenCase getFixtures name vi =
      testCase name $ do
        fs <- getFixtures
        enc vi @?= requireFixture name fs
