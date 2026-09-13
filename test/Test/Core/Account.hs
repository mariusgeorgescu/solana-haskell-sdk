{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Account (tests) where

import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
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

-- | A nonce account's @data@ under @encoding: "jsonParsed"@, as Agave 2.1.16
-- returns it.
nonceParsedJson :: BL.ByteString
nonceParsedJson =
  "{\"parsed\":{\"info\":{\"authority\":\"AkagUZWfCDrEDJTtsuxvAGv9AkhNQAgywJLxyzHPnFQ9\",\"blockhash\":\"EKp95XWYEyvEKaNoZMqaBBBiHPuEyodG9ZC4iHiKHMjj\",\"feeCalculator\":{\"lamportsPerSignature\":\"5000\"}},\"type\":\"initialized\"},\"program\":\"nonce\",\"space\":80}"

-- | The Clock sysvar as a whole @getAccountInfo@ value under
-- @encoding: "jsonParsed"@ (the shape that failed at @$.value.data@).
clockAccountJson :: BL.ByteString
clockAccountJson =
  "{\"data\":{\"parsed\":{\"info\":{\"epoch\":0,\"epochStartTimestamp\":1789286435,\"leaderScheduleEpoch\":1,\"slot\":1367,\"unixTimestamp\":1789287077},\"type\":\"clock\"},\"program\":\"sysvar\",\"space\":40},\"executable\":false,\"lamports\":1169280,\"owner\":\"Sysvar1111111111111111111111111111111111111\",\"rentEpoch\":0,\"space\":40}"

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
         in eitherDecode (encode (AccountDataBinary bs)) === Right (AccountDataBinary bs),
      testCase "AccountData FromJSON decodes a jsonParsed object into AccountDataJSON" $
        case eitherDecode nonceParsedJson of
          Left err -> assertFailure err
          Right (AccountDataJSON prog parsed space) -> do
            prog @?= "nonce"
            space @?= 80
            parseMaybe (withObject "parsed" (.: "type")) parsed @?= Just ("initialized" :: String)
          Right other -> assertFailure ("expected AccountDataJSON, got " <> show other),
      testCase "AccountData FromJSON decodes the jsonParsed base64 fallback pair as binary" $
        eitherDecode "[\"c3lzdGVtX3Byb2dyYW0=\",\"base64\"]" @?= Right (AccountDataBinary "system_program"),
      testCase "AccountData FromJSON rejects a jsonParsed object missing space" $
        assertBool "accepted an object without space" (isLeft (eitherDecode "{\"parsed\":{},\"program\":\"nonce\"}" :: Either String AccountData)),
      testCase "AccountData FromJSON rejects the never-emitted [text, \"jsonParsed\"] pair" $
        assertBool "accepted a jsonParsed pair" (isLeft (eitherDecode "[\"{}\",\"jsonParsed\"]" :: Either String AccountData)),
      testCase "AccountData ToJSON/FromJSON round-trips parsed data" $
        case eitherDecode nonceParsedJson :: Either String AccountData of
          Left err -> assertFailure err
          Right ad -> eitherDecode (encode ad) @?= Right ad,
      testCase "AccountInfo FromJSON accepts a jsonParsed account value" $
        case eitherDecode clockAccountJson :: Either String AccountInfo of
          Left err -> assertFailure err
          Right info -> do
            lamports info @?= Lamport 1169280
            case dataField info of
              AccountDataJSON prog _ space -> (prog @?= "sysvar") >> (space @?= 40)
              other -> assertFailure ("expected AccountDataJSON, got " <> show other)
    ]
