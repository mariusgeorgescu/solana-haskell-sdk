{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Block (tests) where

import Data.Aeson (eitherDecode)
import Network.Solana.Core.Block
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "block"
    [ testCase "BlockHash FromJSON rejects invalid base58" $
        case eitherDecode "\"0OIl\"" :: Either String BlockHash of
          Left _ -> pure ()
          Right bh -> assertFailure ("expected parse failure, got " <> show bh)
    ]
