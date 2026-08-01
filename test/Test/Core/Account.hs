{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Account (tests) where

import Data.Aeson (eitherDecode)
import Network.Solana.Core.Account
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "account"
    [ testCase "AccountData FromJSON rejects an unsupported encoding tag" $
        case eitherDecode "[\"abcd\",\"unknownEncoding\"]" :: Either String AccountData of
          Left _ -> pure ()
          Right ad -> assertFailure ("expected parse failure, got " <> show ad),
      testCase "AccountData FromJSON rejects invalid base58 in a base58-tagged pair" $
        case eitherDecode "[\"0OIl\",\"base58\"]" :: Either String AccountData of
          Left _ -> pure ()
          Right ad -> assertFailure ("expected parse failure, got " <> show ad)
    ]
