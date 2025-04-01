{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.RPC.HTTP.Types where

import Data.Aeson.Types
import Data.Text
import Data.Word
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * RPCResponse

------------------------------------------------------------------------------------------------

type Slot = Word64

data Context = Context
  { apiVersion :: Text,
    contextSlot :: Slot
  }
  deriving (Show, Generic)

instance FromJSON Context where
  parseJSON :: Value -> Parser Context
  parseJSON = withObject "Context" $ \v ->
    Context
      <$> v .: "apiVersion"
      <*> v .: "slot"

data RPCResponse a = RPCResponse
  { context :: Context,
    value :: a
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (RPCResponse a) where
  parseJSON :: (FromJSON a) => Value -> Parser (RPCResponse a)
  parseJSON = withObject "Person" $ \v ->
    RPCResponse
      <$> v .: "context"
      <*> v .: "value"

------------------------------------------------------------------------------------------------

-- *  ConfigurationObject

------------------------------------------------------------------------------------------------

data ConfigurationObject = ConfigurationObject
  { commitment :: Maybe String,
    encoding :: Maybe String,
    dataSlice :: Maybe Object,
    skipPreflight :: Maybe Bool,
    preflightCommitment :: Maybe String,
    maxRetries :: Maybe Int,
    minContextSlot :: Maybe Int,
    searchTransactionHistory :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON)

defaultConfigObject :: ConfigurationObject
defaultConfigObject =
  ConfigurationObject
    { commitment = Nothing,
      encoding = Nothing,
      dataSlice = Nothing,
      skipPreflight = Nothing,
      preflightCommitment = Nothing,
      maxRetries = Nothing,
      minContextSlot = Nothing,
      searchTransactionHistory = Nothing
    }

cfgJustEncodingBase64 :: ConfigurationObject
cfgJustEncodingBase64 =
  defaultConfigObject
    { encoding = Just "base64"
    }
