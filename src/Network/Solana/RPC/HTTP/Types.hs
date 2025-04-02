{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Types
-- Description : Common RPC types used across Solana HTTP JSON-RPC endpoints.
--
-- This module defines shared types used in Solana RPC responses and configuration payloads.
-- It includes generic RPC response envelopes, context metadata, and configuration options
-- for request customization.
--
-- These types are reused across many Solana JSON-RPC methods and should be imported
-- wherever Solana RPC requests or responses are constructed or parsed.
module Network.Solana.RPC.HTTP.Types where

import Data.Aeson.Types
import Data.Text
import Data.Word
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * RPCResponse

------------------------------------------------------------------------------------------------

-- | Alias for a Solana slot number.
type Slot = Word64

-- | Contains contextual information returned with most Solana RPC responses.
--
-- Includes API version and the slot at which the response is valid.
data Context = Context
  { -- | API version string returned by the cluster (e.g. @"1.17.0"@).
    apiVersion :: Text,
    -- | The slot at which the data in the response is valid.
    contextSlot :: Slot
  }
  deriving (Show, Generic)

instance FromJSON Context where
  parseJSON :: Value -> Parser Context
  parseJSON = withObject "Context" $ \v ->
    Context
      <$> v .: "apiVersion"
      <*> v .: "slot"

-- | Generic wrapper for Solana RPC responses that include a context and a value.
--
-- Many Solana RPC methods return a response shaped like:
--
-- > { "context": { ... }, "value": <data> }
data RPCResponse a = RPCResponse
  { -- | Metadata about the cluster state at the time of the response.
    context :: Context,
    -- | The actual value returned by the RPC method.
    value :: a
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (RPCResponse a) where
  parseJSON :: (FromJSON a) => Value -> Parser (RPCResponse a)
  parseJSON = withObject "RPCResponse" $ \v ->
    RPCResponse
      <$> v .: "context"
      <*> v .: "value"

------------------------------------------------------------------------------------------------

-- * ConfigurationObject

------------------------------------------------------------------------------------------------

-- | Configuration options used to customize Solana RPC requests.
--
-- Many RPC methods accept a configuration object to control encoding,
-- commitment level, preflight behavior, and more.
data ConfigurationObject = ConfigurationObject
  { -- | Optional commitment level (e.g. @"finalized"@, @"confirmed"@).
    commitment :: Maybe String,
    -- | Desired encoding for returned data (e.g. @"base58"@, @"base64"@).
    encoding :: Maybe String,
    -- | Optional partial data slice configuration.
    dataSlice :: Maybe Object,
    -- | Whether to skip preflight checks (use with caution).
    skipPreflight :: Maybe Bool,
    -- | Commitment level used during preflight simulation.
    preflightCommitment :: Maybe String,
    -- | Maximum number of retry attempts for transaction submission.
    maxRetries :: Maybe Int,
    -- | Minimum slot that the RPC response must be based on.
    minContextSlot :: Maybe Int,
    -- | Whether to search older confirmed transaction history.
    searchTransactionHistory :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON)

-- | A default configuration object with all fields set to 'Nothing'.
--
-- Use this as a base and override fields selectively.
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

-- | A basic configuration object that only sets the encoding to @"base64"@.
--
-- Useful for methods that require base64-encoded input or output.
cfgJustEncodingBase64 :: ConfigurationObject
cfgJustEncodingBase64 =
  defaultConfigObject
    { encoding = Just "base64"
    }
