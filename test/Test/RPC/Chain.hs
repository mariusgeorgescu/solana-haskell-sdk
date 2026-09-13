{-# LANGUAGE OverloadedStrings #-}

module Test.RPC.Chain (tests) where

import Data.Aeson (eitherDecode)
import Network.Solana.RPC.HTTP.Chain (PerformanceSample (..), SolanaVersion (..), percentilePriorityFee)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Network.Solana.RPC.HTTP.Chain" [percentileTests, sampleTests, versionTests]

percentileTests :: TestTree
percentileTests =
  testGroup
    "percentilePriorityFee"
    [ testCase "empty samples -> 0" $ percentilePriorityFee 0.5 [] @?= 0,
      testCase "all-zero samples -> 0" $ percentilePriorityFee 0.5 [0, 0] @?= 0,
      testCase "single sample" $ percentilePriorityFee 0.5 [5] @?= 5,
      testCase "median of four" $ percentilePriorityFee 0.5 [1, 2, 3, 4] @?= 2,
      testCase "p75 of four" $ percentilePriorityFee 0.75 [1, 2, 3, 4] @?= 3,
      testCase "p100 picks max" $ percentilePriorityFee 1.0 [7, 3] @?= 7,
      testCase "negative p clamps to 0" $ percentilePriorityFee (-1) [9] @?= 9,
      testCase "p above 1 clamps to 1" $ percentilePriorityFee 2 [1, 9] @?= 9,
      testCase "zeros filtered before ranking" $ percentilePriorityFee 0.5 [0, 5, 0, 1] @?= 1
    ]

-- | Upstream @RpcPerfSample.num_non_vote_transactions@ is an @Option@: a
-- current node reports @null@ for samples recorded before it started
-- tracking the field.
sampleTests :: TestTree
sampleTests =
  testGroup
    "PerformanceSample"
    [ testCase "numNonVoteTransactions omitted parses as Nothing" $
        case eitherDecode "{\"slot\":1,\"numTransactions\":2,\"numSlots\":3,\"samplePeriodSecs\":60}" of
          Left err -> assertFailure err
          Right s -> numNonVoteTransactions s @?= Nothing,
      testCase "numNonVoteTransactions null parses as Nothing" $
        case eitherDecode "{\"slot\":1,\"numTransactions\":2,\"numSlots\":3,\"samplePeriodSecs\":60,\"numNonVoteTransactions\":null}" of
          Left err -> assertFailure err
          Right s -> numNonVoteTransactions s @?= Nothing,
      testCase "numNonVoteTransactions present is kept" $
        case eitherDecode "{\"slot\":1,\"numTransactions\":2,\"numSlots\":3,\"samplePeriodSecs\":60,\"numNonVoteTransactions\":4}" of
          Left err -> assertFailure err
          Right s -> numNonVoteTransactions s @?= Just 4
    ]

-- | Upstream @RpcVersionInfo.feature_set@ is an @Option@ as well.
versionTests :: TestTree
versionTests =
  testGroup
    "SolanaVersion"
    [ testCase "feature-set omitted parses as Nothing" $
        eitherDecode "{\"solana-core\":\"2.1.16\"}" @?= Right (SolanaVersion "2.1.16" Nothing),
      testCase "feature-set present is kept" $
        eitherDecode "{\"solana-core\":\"2.1.16\",\"feature-set\":3271415109}" @?= Right (SolanaVersion "2.1.16" (Just 3271415109))
    ]
