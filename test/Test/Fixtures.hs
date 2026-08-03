{-# LANGUAGE OverloadedStrings #-}

module Test.Fixtures
  ( Fixture (..),
    loadFixtures,
    requireFixture,
    RpcFixture (..),
    loadRpcFixtures,
    requireRpcResult,
  )
where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data Fixture = Fixture
  { fixtureName :: String,
    fixtureHex :: T.Text
  }
  deriving (Show)

instance FromJSON Fixture where
  parseJSON = withObject "Fixture" $ \v ->
    Fixture <$> v .: "name" <*> v .: "hex"

loadFixtures :: FilePath -> IO [Fixture]
loadFixtures path = either fail pure =<< eitherDecodeFileStrict path

requireFixture :: String -> [Fixture] -> BS.ByteString
requireFixture name fs =
  case filter ((== name) . fixtureName) fs of
    [f] -> either (error . badHex) id (B16.decode (TE.encodeUtf8 (fixtureHex f)))
    _ -> error ("fixture not found (or duplicated): " <> name)
  where
    badHex err = "bad hex in fixture " <> name <> ": " <> err

-- | One recorded JSON-RPC exchange: the whole response envelope a real node
-- returned, as captured by @tools/rpc-record/record.py@.
data RpcFixture = RpcFixture
  { rpcFixtureName :: String,
    rpcFixtureMethod :: String,
    rpcFixtureResponse :: Value
  }
  deriving (Show)

instance FromJSON RpcFixture where
  parseJSON = withObject "RpcFixture" $ \v ->
    RpcFixture <$> v .: "name" <*> v .: "method" <*> v .: "response"

loadRpcFixtures :: FilePath -> IO [RpcFixture]
loadRpcFixtures path = either fail pure =<< eitherDecodeFileStrict path

-- | The @result@ payload of a recorded response -- what the SDK's parsers
-- actually receive, the JSON-RPC envelope having been stripped by the
-- transport.
requireRpcResult :: String -> [RpcFixture] -> Value
requireRpcResult name fs =
  case filter ((== name) . rpcFixtureName) fs of
    [f] -> either (error . badResult) id (parseEither (withObject "response" (.: "result")) (rpcFixtureResponse f))
    _ -> error ("rpc fixture not found (or duplicated): " <> name)
  where
    badResult err = "no result in rpc fixture " <> name <> ": " <> err
