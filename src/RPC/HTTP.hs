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

import Core.Account (AccountInfo, Lamport)
import Core.Block (BlockCommitment, BlockHash, BlockHeight, BlockProduction, Slot)
import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))

------------------------------------------------------------------------------------------------

-- * HTTP Methods

------------------------------------------------------------------------------------------------

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

-- ** getAccountInfo

------------------------------------------------------------------------------------------------

-- | Returns all information associated with the account of provided 'SolanaPubkey'
-- Returns 'RpcResponse (Maybe AccountInfo)' with value field set to the 'Nothing' if the requested account doesn't exist.
getAccountInfo' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse (Maybe AccountInfo))
getAccountInfo' = do
  remote "getAccountInfo"
{-# INLINE getAccountInfo' #-}

-- | Returns all information associated with the account of provided 'SolanaPubkey'
-- Returns '(Maybe AccountInfo)' with value field set to the 'Nothing' if the requested account doesn't exist.
getAccountInfo :: (JsonRpc m) => SolanaPublicKey -> m (Maybe AccountInfo)
getAccountInfo pubKey = value <$> getAccountInfo' pubKey
{-# INLINE getAccountInfo #-}

------------------------------------------------------------------------------------------------

-- ** getBalance

------------------------------------------------------------------------------------------------

-- | Get the lamport balance of the account of provided 'SolanaPubkey'.
-- Returns 'RpcResponse Lamport' with value field set to the 'Lamport' balance.
getBalance' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse Lamport)
getBalance' = do
  remote "getBalance"
{-# INLINE getBalance' #-}

-- | Get the lamport balance of the account of provided 'SolanaPubkey'.
-- Returns 'Lamport' the balance.
getBalance :: (JsonRpc m) => SolanaPublicKey -> m Lamport
getBalance pubKey = value <$> getBalance' pubKey
{-# INLINE getBalance #-}

------------------------------------------------------------------------------------------------

-- ** getBlock

------------------------------------------------------------------------------------------------

--- TODO
--- TODO
--- TODO
--- TODO
--- TODO

------------------------------------------------------------------------------------------------

-- ** getBlockCommitment

------------------------------------------------------------------------------------------------

-- | Get the block commitment based on the block number 'Solt'.
-- Returns 'BlockCommitment' for particular block
getBlockCommitment :: (JsonRpc m) => Slot -> m BlockCommitment
getBlockCommitment = do
  remote "getBlockCommitment"
{-# INLINE getBlockCommitment #-}

------------------------------------------------------------------------------------------------

-- ** getBlockHeight

------------------------------------------------------------------------------------------------

-- | Returns the current block height of the node
getBlockHeight :: (JsonRpc m) => m BlockHeight
getBlockHeight = do
  remote "getBlockHeight"
{-# INLINE getBlockHeight #-}

------------------------------------------------------------------------------------------------

-- ** getBlockProduction

------------------------------------------------------------------------------------------------

-- | Returns recent block production information from the current or previous epoch.
-- Returns 'RpcResponse BlockProduction' with value field set to the 'BlockProduction'.
getBlockProduction' :: (JsonRpc m) => m (RPCResponse BlockProduction)
getBlockProduction' = do
  remote "getBlockProduction"
{-# INLINE getBlockProduction' #-}

-- | Returns recent block production information from the current or previous epoch.
getBlockProduction :: (JsonRpc m) => m BlockProduction
getBlockProduction = value <$> getBlockProduction'
{-# INLINE getBlockProduction #-}

------------------------------------------------------------------------------------------------

-- * HTTP Methods

------------------------------------------------------------------------------------------------

type LastValidBlockHeight = Int

data LatestBlockHashResp
  = LatestBlockHashResp
      BlockHash
      LastValidBlockHeight

instance FromJSON LatestBlockHashResp where
  parseJSON :: Value -> Parser LatestBlockHashResp
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

requestAirdrop :: (JsonRpc m) => SolanaPublicKey -> Lamport -> m String
requestAirdrop = do
  remote "requestAirdrop"

data RpcSendTransactionConfig = RpcSendTransactionConfig
  { encoding :: String,
    skipPreflight :: Bool,
    preflightCommitment :: String,
    maxRetries :: Int,
    minContextSlot :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

defaultRpcSendTransactionConfig :: RpcSendTransactionConfig
defaultRpcSendTransactionConfig =
  RpcSendTransactionConfig
    { encoding = "base64",
      skipPreflight = True,
      preflightCommitment = "finalized",
      maxRetries = 0,
      minContextSlot = 0
    }

sendTransaction :: (JsonRpc m) => String -> m String
sendTransaction tx = sendTransaction' tx defaultRpcSendTransactionConfig

sendTransaction' :: (JsonRpc m) => String -> RpcSendTransactionConfig -> m String
sendTransaction' = do
  remote "sendTransaction"