{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import Core.Account (Lamport)
import Core.Block (BlockHash)
import Core.Crypto
import Data.Aeson
import Data.Aeson.Types (Parser)
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
  parseJSON :: (FromJSON a) => Value -> Parser (RPCResponse a)
  parseJSON = withObject "Person" $ \v ->
    RPCResponse
      <$> v .: "context"
      <*> v .: "value"

------------------------------------------------------------------------------------------------

-- * HTTP Methods

------------------------------------------------------------------------------------------------

-- | Returns the lamport balance of the account of provided SolanaPubkey.
getBalance :: (JsonRpc m) => String -> m (RPCResponse Lamport)
{-# INLINE getBalance #-}
getBalance = do
  remote "getBalance"

------------------------------------------------------------------------------------------------

-- * HTTP Methods

------------------------------------------------------------------------------------------------
type LastValidBlockHeight = Int

data LatestBlockHashResp = LatestBlockHashResp BlockHash LastValidBlockHeight

instance FromJSON LatestBlockHashResp where

  parseJSON = withObject "LatestBlockHashResp" $ \v ->
    LatestBlockHashResp
      <$> v .: "blockhash"
      <*> v .: "lastValidBlockHeight"


getLatestBlockhash' :: (JsonRpc m) => m (RPCResponse LatestBlockHashResp)
{-# INLINE getLatestBlockhash' #-}
getLatestBlockhash' = do
  remote "getLatestBlockhash"

getLatestBlockhash :: (JsonRpc m) => m (RPCResponse BlockHash)
getLatestBlockhash = do
  RPCResponse v (LatestBlockHashResp bh _) <- getLatestBlockhash'
  return $ RPCResponse v bh