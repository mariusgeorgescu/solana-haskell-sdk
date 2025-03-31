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
import Core.Crypto (SolanaPrivateKey, SolanaPublicKey, SolanaSignature)
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

-- | Returns identity and transaction information about a confirmed block in the ledger.
getBlock :: (JsonRpc m) => Slot -> m BlockInfo
getBlock = do
  remote "getBlock"
{-# INLINE getBlock #-}

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

-- * getSignatureStatuses

------------------------------------------------------------------------------------------------

-- | Returns the statuses of a list of @TransactionSignatureStatus@.
-- Each signature must be a TxId (the first signature in a transaction, which can be used to uniquely identify
-- the transaction across the complete ledger).
getSignatureStatuses' :: (JsonRpc m) => [SolanaSignature] -> SearchTransactionHistory -> m (RPCResponse [Maybe TransactionSignatureStatus])
getSignatureStatuses' = do
  remote "getSignatureStatuses"
{-# INLINE getSignatureStatuses' #-}

-- | Returns the statuses of a list of @TransactionSignatureStatus@.
-- Each signature must be a TxId (the first signature in a transaction, which can be used to uniquely identify
-- the transaction across the complete ledger).
getSignatureStatuses :: (JsonRpc m) => [SolanaSignature] -> m [Maybe TransactionSignatureStatus]
getSignatureStatuses sigs = value <$> getSignatureStatuses' sigs (SearchTransactionHistory True)
{-# INLINE getSignatureStatuses #-}

------------------------------------------------------------------------------------------------

-- * getSlot

------------------------------------------------------------------------------------------------

-- | Returns the slot that has reached the given or default commitment level
getSlot :: (JsonRpc m) => m Slot
getSlot = do
  remote "getSlot"

------------------------------------------------------------------------------------------------

-- * getSlotLeader

------------------------------------------------------------------------------------------------

-- | Returns the current slot leader
getSlotLeader :: (JsonRpc m) => m SolanaPublicKey
getSlotLeader = do
  remote "getSlotLeader"

------------------------------------------------------------------------------------------------

-- * getSlotLeaders

------------------------------------------------------------------------------------------------

-- | Returns the slot leaders for a given slot range
-- Receives the start 'Slot' and a limit as an 'Int' (between 1 and 5,000).
getSlotLeaders :: (JsonRpc m) => Slot -> Int -> m [SolanaPublicKey]
getSlotLeaders = do
  remote "getSlotLeaders"

------------------------------------------------------------------------------------------------

-- * getStakeMinimumDelegation

------------------------------------------------------------------------------------------------

-- | Returns the stake minimum delegation, in lamports.
-- Returns @RpcResponse Lamport@ with value field set to @Lamport@.
getStakeMinimumDelegation' :: (JsonRpc m) => m (RPCResponse Lamport)
getStakeMinimumDelegation' = do
  remote "getStakeMinimumDelegation"

-- | Returns the stake minimum delegation, in lamports.
getStakeMinimumDelegation :: (JsonRpc m) => m Lamport
getStakeMinimumDelegation = value <$> getStakeMinimumDelegation'

------------------------------------------------------------------------------------------------

-- * getStakeMinimgetSupplyumDelegation

------------------------------------------------------------------------------------------------

-- | Returns information about the current supply.
-- Returns @RpcResponse SolanaSupply@ with value field set to @SolanaSupply@.
getSupply' :: (JsonRpc m) => m (RPCResponse SolanaSupply)
getSupply' = do
  remote "getSupply"

-- | Returns information about the current supply.
-- Returns @RpcResponse SolanaSupply@ with value field set to @SolanaSupply@.
getSupply :: (JsonRpc m) => m SolanaSupply
getSupply = value <$> getSupply'

------------------------------------------------------------------------------------------------

-- * getTokenAccountBalance

------------------------------------------------------------------------------------------------

-- | Returns the token balance of an SPL Token account.
-- Returns @RpcResponse AmmountObject@ with value field set to @AmmountObject@.
getTokenAccountBalance' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse AmmountObject)
getTokenAccountBalance' = do
  remote "getTokenAccountBalance"

-- | Returns the token balance of an SPL Token account.
getTokenAccountBalance :: (JsonRpc m) => SolanaPublicKey -> m AmmountObject
getTokenAccountBalance = fmap value . getTokenAccountBalance'

------------------------------------------------------------------------------------------------

-- * getTokenAccountsByDelegate

------------------------------------------------------------------------------------------------

-- | Returns all SPL Token accounts by approved Delegate.
getTokenAccountsByDelegate :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> ConfigurationObject -> m (RPCResponse [Account])
getTokenAccountsByDelegate = do
  remote "getTokenAccountsByDelegate"
{-# INLINE getTokenAccountsByDelegate #-}

------------------------------------------------------------------------------------------------

-- * getTokenAccountsByOwner

------------------------------------------------------------------------------------------------

-- | Returns all SPL Token accounts by token owner.
getTokenAccountsByOwner' :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> ConfigurationObject -> m (RPCResponse [Account])
getTokenAccountsByOwner' = do
  remote "getTokenAccountsByOwner"
{-# INLINE getTokenAccountsByOwner' #-}

-- | Returns all SPL Token accounts by token owner.
getTokenAccountsByOwner :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> m [Account]
getTokenAccountsByOwner pk pkwv = value <$> getTokenAccountsByOwner' pk pkwv cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwner #-}

-- | Returns all SPL Token accounts by token owner and mint.
getTokenAccountsByOwnerAndMint :: (JsonRpc m) => SolanaPublicKey -> SolanaPublicKey -> m [Account]
getTokenAccountsByOwnerAndMint pk pkwv = value <$> getTokenAccountsByOwner' pk (Mint pkwv) cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwnerAndMint #-}

-- | Returns all SPL Token accounts by token owner and program.
getTokenAccountsByOwnerAndProgram :: (JsonRpc m) => SolanaPublicKey -> SolanaPublicKey -> m [Account]
getTokenAccountsByOwnerAndProgram pk pkwv = value <$> getTokenAccountsByOwner' pk (Program pkwv) cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwnerAndProgram #-}

------------------------------------------------------------------------------------------------

-- * getTokenLargestAccounts

------------------------------------------------------------------------------------------------

-- | Returns the token balance of an SPL Token account.
-- Returns @RpcResponse AmmountObjectWithAddr@ with value field set to @AmmountObjectWithAddr@.
getTokenLargestAccounts' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse [AmmountObjectWithAddr])
getTokenLargestAccounts' = do
  remote "getTokenLargestAccounts"

-- | Returns the token balance of an SPL Token account.
getTokenLargestAccounts :: (JsonRpc m) => SolanaPublicKey -> m [AmmountObjectWithAddr]
getTokenLargestAccounts = fmap value . getTokenLargestAccounts'

------------------------------------------------------------------------------------------------

-- * getStakeMinimgetSupplyumDelegation

------------------------------------------------------------------------------------------------

-- | Returns the total supply of an SPL Token type.
-- Returns @RpcResponse AmmountObject@ with value field set to @AmmountObject@.
getTokenSupply' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse AmmountObject)
getTokenSupply' = do
  remote "getTokenSupply"

-- | Returns information about the current supply.
getTokenSupply :: (JsonRpc m) => SolanaPublicKey -> m AmmountObject
getTokenSupply = fmap value . getTokenSupply'

------------------------------------------------------------------------------------------------

-- * getTransaction

------------------------------------------------------------------------------------------------

-- | Returns information about the current supply.
getTransaction :: (JsonRpc m) => SolanaSignature -> m TransactionResult
getTransaction = do
  remote "getTransaction"

------------------------------------------------------------------------------------------------

-- * getTransactionCount

------------------------------------------------------------------------------------------------

-- | Returns the current Transaction count from the ledger
getTransactionCount :: (JsonRpc m) => m Slot
getTransactionCount = do
  remote "getTransactionCount"

------------------------------------------------------------------------------------------------

-- * getVersion

------------------------------------------------------------------------------------------------

-- | Returns the current Solana version running on the node.
getVersion :: (JsonRpc m) => m SolanaVersion
getVersion = do
  remote "getVersion"

------------------------------------------------------------------------------------------------

-- * getVoteAccounts

------------------------------------------------------------------------------------------------
--
--
-- TODO
--
--

------------------------------------------------------------------------------------------------

-- * isBlockhashValid

------------------------------------------------------------------------------------------------

-- | Returns whether a blockhash is still valid or not.
isBlockhashValid' :: (JsonRpc m) => BlockHash -> m (RPCResponse Bool)
isBlockhashValid' = do
  remote "isBlockhashValid"

-- | Returns whether a blockhash is still valid or not.
isBlockhashValid :: (JsonRpc m) => BlockHash -> m Bool
isBlockhashValid = fmap value . isBlockhashValid'

------------------------------------------------------------------------------------------------

-- * minimumLedgerSlot

------------------------------------------------------------------------------------------------

-- | Returns the lowest slot that the node has information about in its ledger.
minimumLedgerSlot :: (JsonRpc m) => m Slot
minimumLedgerSlot = do
  remote "minimumLedgerSlot"

------------------------------------------------------------------------------------------------

-- * requestAirdrop

------------------------------------------------------------------------------------------------

requestAirdrop :: (JsonRpc m) => SolanaPublicKey -> Lamport -> m SolanaSignature
requestAirdrop = do
  remote "requestAirdrop"

------------------------------------------------------------------------------------------------

-- * sendTransaction

------------------------------------------------------------------------------------------------

defaultRpcSendTransactionConfig :: ConfigurationObject
defaultRpcSendTransactionConfig =
  defaultConfigObject
    { encoding = Just "base64",
      skipPreflight = Just True,
      preflightCommitment = Just "finalized",
      maxRetries = Just 0,
      minContextSlot = Just 0
    }

sendTransaction :: (JsonRpc m) => String -> m String
sendTransaction tx = sendTransaction' tx defaultRpcSendTransactionConfig

sendTransaction' :: (JsonRpc m) => String -> ConfigurationObject -> m String
sendTransaction' = do
  remote "sendTransaction"

------------------------------------------------------------------------------------------------

-- * simulateTransaction

------------------------------------------------------------------------------------------------
--
--
-- TODO
--
--
