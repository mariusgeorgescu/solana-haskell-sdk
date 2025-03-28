{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Network.Ethereum.Web3.Eth
-- Copyright   :  Marius Georgescu
-- License     :  GNU GENERAL PUBLIC LICENSE
--
-- Maintainer  :  georgescumarius@live.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Solana JSON-RPC API methods
module RPC.HTTP where

import Core.Crypto
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))

data Context = Context
  { apiVersion :: Text,
    slot :: Int
  }
  deriving (Show, Generic)

instance FromJSON Context

data RPCResponse a = RPCResponse
  { context :: Context,
    value :: a
  }
  deriving (Show)

instance (FromJSON a) => FromJSON (RPCResponse a) where
  parseJSON = withObject "Person" $ \v ->
    RPCResponse
      <$> v .: "context"
      <*> v .: "value"

-- | Returns the lamport balance of the account of provided SolanaPubkey.
getBalance :: (JsonRpc m) => String -> m (RPCResponse Integer)
{-# INLINE getBalance #-}
getBalance = do
  remote "getBalance"