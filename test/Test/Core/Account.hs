{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Account (tests) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Word (Word8)
import Network.Solana.Core.Account
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | The Rent sysvar's 17 data bytes (the cluster-independent defaults), as
-- Agave returns them: bare base58 under the default encoding, a tagged pair
-- under @base64@.
rentBytes :: BS.ByteString
rentBytes = BS.pack [152, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 50]

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
          Right ad -> assertFailure ("expected parse failure, got " <> show ad),
      testCase "AccountData FromJSON decodes a bare string as base58 (the node's default binary encoding)" $
        eitherDecode "\"2RsdFKVyfoKRJwMEPcvasdsF\"" @?= Right (AccountDataBinary rentBytes),
      testCase "bare string and base58-tagged pair decode identically" $ do
        eitherDecode "\"1111\"" @?= Right (AccountDataBinary (BS.replicate 4 0))
        eitherDecode "[\"1111\",\"base58\"]" @?= Right (AccountDataBinary (BS.replicate 4 0)),
      testCase "base64-tagged pair still decodes as base64" $
        eitherDecode "[\"mA0AAAAAAAAAAAAAAAAAQDI=\",\"base64\"]" @?= Right (AccountDataBinary rentBytes),
      testCase "bare empty string decodes to empty account data" $
        eitherDecode "\"\"" @?= Right (AccountDataBinary BS.empty),
      testCase "AccountData FromJSON rejects a bare string that is not base58" $
        case eitherDecode "\"0OIl\"" :: Either String AccountData of
          Left err -> assertBool ("expected \"invalid base58\" in: " <> err) ("invalid base58" `isInfixOf` err)
          Right ad -> assertFailure ("expected parse failure, got " <> show ad),
      testProperty "AccountData JSON round-trip (ToJSON emits the bare base58 form)" $ \(ws :: [Word8]) ->
        let bs = BS.pack ws
         in eitherDecode (encode (AccountDataBinary bs)) === Right (AccountDataBinary bs)
    ]
