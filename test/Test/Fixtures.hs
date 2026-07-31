{-# LANGUAGE OverloadedStrings #-}

module Test.Fixtures
  ( Fixture (..),
    loadFixtures,
    requireFixture,
  )
where

import Data.Aeson
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
