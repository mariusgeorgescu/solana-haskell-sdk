{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Block (tests) where

import Data.Aeson (eitherDecode)
import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LC8
import Network.Solana.Core.Block
import Network.Solana.Core.Crypto (toBase58String)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "block"
    [ testCase "BlockHash FromJSON rejects invalid base58" $
        case eitherDecode "\"0OIl\"" :: Either String BlockHash of
          Left _ -> pure ()
          Right bh -> assertFailure ("expected parse failure, got " <> show bh),
      testCase "BlockHash FromJSON rejects valid base58 of the wrong length" $
        let wrongLengthBase58 = toBase58String (BS.replicate 5 7)
            json = LC8.pack (show wrongLengthBase58)
         in case eitherDecode json :: Either String BlockHash of
              Left _ -> pure ()
              Right bh -> assertFailure ("expected parse failure, got " <> show bh),
      testCase "BlockHash Binary round-trip (32-byte hash)" $
        let bh = BlockHash (BS.replicate 32 7)
         in decode (encode bh) @?= bh
    ]
