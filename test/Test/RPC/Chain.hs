module Test.RPC.Chain (tests) where

import Network.Solana.RPC.HTTP.Chain (percentilePriorityFee)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
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
