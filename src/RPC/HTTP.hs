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

import Control.Exception (throw)
import Core.Account (Account, AccountInfo, Lamport)
import Core.Block
import Core.Crypto (SolanaPrivateKey, SolanaPublicKey)
import Core.Instruction
import Core.Message (newMessageToBase64String)
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.Types

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

-- * getBlocks

------------------------------------------------------------------------------------------------

-- | Returns a list of confirmed blocks between two slots
-- An array of integers listing confirmed blocks between start_slot and either end_slot
-- if provided, or latest confirmed slot, inclusive. Max range allowed is 500,000 slots.
getBlocks :: (JsonRpc m) => Slot -> Maybe Slot -> m [Int]
getBlocks = do
  remote "getBlocks"
{-# INLINE getBlocks #-}

------------------------------------------------------------------------------------------------

-- * getBlocksWithLimit

------------------------------------------------------------------------------------------------

-- | Returns a list of confirmed blocks starting at the given slot.
-- An array of integers listing confirmed blocks starting at start_slot for up to limit blocks, inclusive.
getBlocksWithLimit :: (JsonRpc m) => Slot -> Int -> m [Int]
getBlocksWithLimit = do
  remote "getBlocksWithLimit"
{-# INLINE getBlocksWithLimit #-}

------------------------------------------------------------------------------------------------

-- * getBlockTime

------------------------------------------------------------------------------------------------

-- | Returns the estimated production time of a block (as Unix timestamp / seconds since the Unix epoch).
-- Each validator reports their UTC time to the ledger on a regular interval by intermittently adding a timestamp to a Vote for a particular block.
-- A requested block's time is calculated from the stake-weighted mean of the Vote timestamps in a set of recent blocks recorded on the ledger.
getBlockTime :: (JsonRpc m) => Slot -> m Int
getBlockTime = do
  remote "getBlockTime"
{-# INLINE getBlockTime #-}

------------------------------------------------------------------------------------------------

-- * getClusterNodes

------------------------------------------------------------------------------------------------

-- | Returns information about all the nodes participating in the cluster 'ClusterNodes'.
getClusterNodes :: (JsonRpc m) => m [ClusterNodes]
getClusterNodes = do
  remote "getClusterNodes"
{-# INLINE getClusterNodes #-}

------------------------------------------------------------------------------------------------

-- * getEpochInfo

------------------------------------------------------------------------------------------------

-- | Returns information about the current epoch.
getEpochInfo :: (JsonRpc m) => m EpochInfo
getEpochInfo = do
  remote "getEpochInfo"
{-# INLINE getEpochInfo #-}

------------------------------------------------------------------------------------------------

-- * getEpochSchedule

------------------------------------------------------------------------------------------------

-- | Returns the epoch schedule information from this cluster's genesis config.
getEpochSchedule :: (JsonRpc m) => m EpochSchedule
getEpochSchedule = do
  remote "getEpochSchedule"
{-# INLINE getEpochSchedule #-}

------------------------------------------------------------------------------------------------

-- * getFeeForMessage

------------------------------------------------------------------------------------------------

-- | Get the fee the network will charge for a particular Message.
-- Returns 'RpcResponse Lamport' with value field set  to the fee corresponding to the message at the specified blockhash
getFeeForMessage' :: (JsonRpc m) => String -> m (RPCResponse (Maybe Int))
getFeeForMessage' = do
  remote "getFeeForMessage"
{-# INLINE getFeeForMessage' #-}

-- | Get the fee the network will charge for a particular Message.
-- Returns the fee corresponding to the message at the specified blockhash
getFeeForMessage :: (JsonRpc m) => String -> m (Maybe Int)
getFeeForMessage = fmap value . getFeeForMessage'
{-# INLINE getFeeForMessage #-}

------------------------------------------------------------------------------------------------

-- * getFirstAvailableBlock

------------------------------------------------------------------------------------------------

-- | Returns the slot of the lowest confirmed block that has not been purged from the ledger.
getFirstAvailableBlock :: (JsonRpc m) => m Slot
getFirstAvailableBlock = do
  remote "getFirstAvailableBlock"
{-# INLINE getFirstAvailableBlock #-}

------------------------------------------------------------------------------------------------

-- * getGenesisHash

------------------------------------------------------------------------------------------------

-- | Returns the genesis hash.
getGenesisHash :: (JsonRpc m) => m BlockHash
getGenesisHash = do
  remote "getGenesisHash"
{-# INLINE getGenesisHash #-}

------------------------------------------------------------------------------------------------

-- * getHealth

------------------------------------------------------------------------------------------------

-- | Returns the current health of the node.
-- A healthy node is one that is within HEALTH_CHECK_SLOT_DISTANCE slots of the latest cluster confirmed slot.
getHealth :: (JsonRpc m) => m String
getHealth = do
  remote "getHealth"
{-# INLINE getHealth #-}

------------------------------------------------------------------------------------------------

-- * getHighestSnapshotSlot

------------------------------------------------------------------------------------------------

-- | Returns the highest slot information that the node has snapshots for.
-- This will find the highest full snapshot slot, and the highest incremental snapshot slot based on the full snapshot slot, if there is one.
getHighestSnapshotSlot :: (JsonRpc m) => m HighestSnapshotSlot
getHighestSnapshotSlot = do
  remote "getHighestSnapshotSlot"
{-# INLINE getHighestSnapshotSlot #-}

------------------------------------------------------------------------------------------------

-- * getIdentity

------------------------------------------------------------------------------------------------

-- | Returns the 'NodeIdentity'for the current node.
getIdentity' :: (JsonRpc m) => m NodeIdentity
getIdentity' = do
  remote "getIdentity"
{-# INLINE getIdentity' #-}

-- | Returns the identity pubkey for the current node.
getIdentity :: (JsonRpc m) => m SolanaPublicKey
getIdentity = identity <$> getIdentity'
{-# INLINE getIdentity #-}

------------------------------------------------------------------------------------------------

-- * getInflationGovernor

------------------------------------------------------------------------------------------------

-- | Returns the current inflation governor.
getInflationGovernor :: (JsonRpc m) => m InflationGovernor
getInflationGovernor = do
  remote "getInflationGovernor"
{-# INLINE getInflationGovernor #-}

------------------------------------------------------------------------------------------------

-- * getInflationRate

------------------------------------------------------------------------------------------------

-- | Returns the specific inflation values for the current epoch.
getInflationRate :: (JsonRpc m) => m InflationRate
getInflationRate = do
  remote "getInflationRate"
{-# INLINE getInflationRate #-}

------------------------------------------------------------------------------------------------

-- * getInflationReward

------------------------------------------------------------------------------------------------

-- | Returns the inflation / staking reward for a list of addresses for an epoch.
getInflationReward :: (JsonRpc m) => [String] -> m [Maybe InflationReward]
getInflationReward = do
  remote "getInflationReward"
{-# INLINE getInflationReward #-}

------------------------------------------------------------------------------------------------

-- * getLargestAccounts

------------------------------------------------------------------------------------------------

-- | Returns the 20 largest accounts, by lamport balance (results may be cached up to two hours).
-- Returns 'RpcResponse [AddressAndLamports]' with value field set to a list of 'AddressAndLamports'.
getLargestAccounts' :: (JsonRpc m) => m (RPCResponse [AddressAndLamports])
getLargestAccounts' = do
  remote "getLargestAccounts"
{-# INLINE getLargestAccounts' #-}

-- | Returns the 20 largest accounts, by lamport balance (results may be cached up to two hours).
-- | Returns a list of pairs of address and balance.
getLargestAccounts :: (JsonRpc m) => m [(SolanaPublicKey, Lamport)]
getLargestAccounts = fmap (liftA2 (,) address lamports) . value <$> getLargestAccounts'
{-# INLINE getLargestAccounts #-}

------------------------------------------------------------------------------------------------

-- * getLatestBlockhash

------------------------------------------------------------------------------------------------

-- | Get the hash and the height of the latest block.
-- Returns 'RpcResponse LatestBlockHash' with value field set to a list of 'LatestBlockHash'.
getLatestBlockhash' :: (JsonRpc m) => m (RPCResponse LatestBlockHash)
getLatestBlockhash' = do
  remote "getLatestBlockhash"
{-# INLINE getLatestBlockhash' #-}

-- | Get the hash and the height of the latest block as 'LatestBlockHash'.
getLatestBlockhash :: (JsonRpc m) => m LatestBlockHash
getLatestBlockhash = value <$> getLatestBlockhash'
{-# INLINE getLatestBlockhash #-}

-- | Get the 'BlockHash' of the latest block.
getTheLatestBlockhash :: (JsonRpc m) => m BlockHash
getTheLatestBlockhash = blockhash <$> getLatestBlockhash
{-# INLINE getTheLatestBlockhash #-}

------------------------------------------------------------------------------------------------

-- * getLatestBlockhagetLeaderSchedulesh

------------------------------------------------------------------------------------------------

-- | Get the leader schedule for an epoch.
-- Fetch the leader schedule for the epoch that corresponds to the provided slot.
-- If unspecified, the leader schedule for the current epoch is fetched.
getLeaderSchedule :: (JsonRpc m) => Maybe Slot -> m (Maybe LeaderSchedule)
getLeaderSchedule = do
  remote "getLeaderSchedule"
{-# INLINE getLeaderSchedule #-}

------------------------------------------------------------------------------------------------

-- * getMaxRetransmitSlot

------------------------------------------------------------------------------------------------

-- | Get the max slot seen from retransmit stage.
getMaxRetransmitSlot :: (JsonRpc m) => m Slot
getMaxRetransmitSlot = do
  remote "getMaxRetransmitSlot"
{-# INLINE getMaxRetransmitSlot #-}

------------------------------------------------------------------------------------------------

-- * getMaxShredInsertSlot

------------------------------------------------------------------------------------------------

-- | Get the max slot seen from after shred insert.
getMaxShredInsertSlot :: (JsonRpc m) => m Slot
getMaxShredInsertSlot = do
  remote "getMaxShredInsertSlot"
{-# INLINE getMaxShredInsertSlot #-}

------------------------------------------------------------------------------------------------

-- * getMinimumBalanceForRentExemption

------------------------------------------------------------------------------------------------

-- | Get the minimum balance required to make account rent exempt.
--  Receives the Account's data length and returns the balance balance in 'Lamport'.
getMinimumBalanceForRentExemption :: (JsonRpc m) => Int -> m Lamport
getMinimumBalanceForRentExemption = do
  remote "getMinimumBalanceForRentExemption"
{-# INLINE getMinimumBalanceForRentExemption #-}

------------------------------------------------------------------------------------------------

-- * getMultipleAccounts

------------------------------------------------------------------------------------------------

-- | Returns list of accounts based on their 'SolanaPubkey'
-- Returns @RpcResponse [(Maybe AccountInfo)]@ with value field set to @[(Maybe AccountInfo)]@
-- If the account of a given 'SolanaPubkey' doesn't exist, the coresponding element in the list is 'Nothing'.
getMultipleAccounts' :: (JsonRpc m) => [SolanaPublicKey] -> m (RPCResponse [Maybe AccountInfo])
getMultipleAccounts' = do
  remote "getMultipleAccounts"
{-# INLINE getMultipleAccounts' #-}

------------------------------------------------------------------------------------------------

-- | Returns list of accounts based on their 'SolanaPubkey'
-- If the account of a given 'SolanaPubkey' doesn't exist, the coresponding element in the list is 'Nothing'.
getMultipleAccounts :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe AccountInfo]
getMultipleAccounts = fmap value . getMultipleAccounts'
{-# INLINE getMultipleAccounts #-}

------------------------------------------------------------------------------------------------

-- * getProgramAccounts

------------------------------------------------------------------------------------------------

-- | Returns list of accounts owned by the provided program 'SolanaPubkey'
-- Returns a list of 'Account.
-- If the account of a given 'SolanaPubkey' doesn't exist, the list is empty.
getProgramAccounts :: (JsonRpc m) => SolanaPublicKey -> m [Account]
getProgramAccounts = do
  remote "getProgramAccounts"
{-# INLINE getProgramAccounts #-}

------------------------------------------------------------------------------------------------

-- * getRecentPerformanceSamples

------------------------------------------------------------------------------------------------

-- | Returns a list of recent 'PerformanceSample', in reverse slot order.
-- Performance samples are taken every 60 seconds
-- and include the number of transactions and slots that occur in a given time window.
-- Takes as optional parameter the number of samples to return (throws exception if is > 720).
getRecentPerformanceSamples' :: (JsonRpc m) => Maybe Int -> m [PerformanceSample]
getRecentPerformanceSamples' = do
  remote "getRecentPerformanceSamples"
{-# INLINE getRecentPerformanceSamples' #-}

-- | Returns a list of recent 'PerformanceSample', in reverse slot order.
-- Performance samples are taken every 60 seconds
-- and include the number of transactions and slots that occur in a given time window.
-- Takes as optional parameter the number of samples to return (value capped at 720).
getRecentPerformanceSamples :: (JsonRpc m) => Maybe Int -> m [PerformanceSample]
getRecentPerformanceSamples mi = getRecentPerformanceSamples' (min 720 <$> mi)
{-# INLINE getRecentPerformanceSamples #-}

------------------------------------------------------------------------------------------------

-- * getRecentPrioritizationFees

------------------------------------------------------------------------------------------------

-- | Returns a list of prioritization fees from recent blocks.
-- Takes and optional list of account addresses (up to a maximum of 128 addresses) @[SolanaPublicKey]@ .
-- If this parameter is provided, the response will reflect a fee to land a transaction locking all of the provided accounts as writable.
getRecentPrioritizationFees :: (JsonRpc m) => Maybe [SolanaPublicKey] -> m [PrioritizationFee]
getRecentPrioritizationFees = do
  remote "getRecentPrioritizationFees"
{-# INLINE getRecentPrioritizationFees #-}

------------------------------------------------------------------------------------------------

-- * getSignaturesForAddress

------------------------------------------------------------------------------------------------

-- | Returns signatures for confirmed transactions that include the given address in their 'accountKeys' list.
-- Returns signatures backwards in time from the provided signature or most recent confirmed block
getSignaturesForAddress :: (JsonRpc m) => SolanaPublicKey -> m [TransactionSignatureInformation]
getSignaturesForAddress = do
  remote "getSignaturesForAddress"
{-# INLINE getSignaturesForAddress #-}

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- * requestAirdrop

------------------------------------------------------------------------------------------------

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
