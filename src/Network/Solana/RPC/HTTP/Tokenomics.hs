{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Tokenomics
-- Description : Solana RPC methods for querying inflation, staking, supply, and vote account data.
--
-- This module exposes Solana JSON-RPC methods for interacting with protocol-level
-- tokenomics: inflation schedules, rewards, token supply, staking parameters, and
-- validator vote accounts.
--
-- Useful for dashboards, analytics tools, staking services, or indexers.
module Network.Solana.RPC.HTTP.Tokenomics where

import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import Data.Word
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getInflationGovernor

------------------------------------------------------------------------------------------------

-- | Returns the current inflation governor parameters.
--
-- These parameters define how inflation is distributed and how it changes over time.
getInflationGovernor :: (JsonRpc m) => m InflationGovernor
getInflationGovernor = do
  remote "getInflationGovernor"
{-# INLINE getInflationGovernor #-}

-- | Parameters defining the inflation configuration.
data InflationGovernor = InflationGovernor
  { -- | Percentage of inflation allocated to the foundation.
    foundation :: Double,
    -- | Duration of the foundation pool inflation (in years).
    foundationTerm :: Double,
    -- | Initial inflation rate at genesis (percentage).
    initial :: Double,
    -- | Rate per year at which inflation is lowered. (Rate reduction is derived using the target slot time in genesis config)
    taper :: Double,
    -- | Terminal inflation rate (percentage).
    terminal :: Double
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getInflationRate

------------------------------------------------------------------------------------------------

-- | Returns the inflation rate values for the current epoch.
getInflationRate :: (JsonRpc m) => m InflationRate
getInflationRate = do
  remote "getInflationRate"
{-# INLINE getInflationRate #-}

-- | Inflation breakdown for the current epoch.
data InflationRate = InflationRate
  { -- | Total inflation rate (percentage).
    totalInflation :: Double,
    -- | Portion allocated to validators.
    validatorInflation :: Double,
    -- | Portion allocated to the foundation.
    foundationInflation :: Double,
    -- | Epoch index for which these values apply.
    epochInflation :: Int64
  }
  deriving (Show)

instance FromJSON InflationRate where
  parseJSON :: Value -> Parser InflationRate
  parseJSON = withObject "InflationRate" $ \v ->
    InflationRate
      <$> v .: "total"
      <*> v .: "validator"
      <*> v .: "foundation"
      <*> v .: "epoch"

------------------------------------------------------------------------------------------------

-- * getInflationReward

------------------------------------------------------------------------------------------------

-- | Returns the inflation reward for the specified accounts during an epoch.
--
-- Takes a list of public keys and returns a list of optional rewards.
-- Entries may be 'Nothing' if the reward could not be determined for the given account.
getInflationReward :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe InflationReward]
getInflationReward = do
  remote "getInflationReward"
{-# INLINE getInflationReward #-}

-- | Contains the inflation reward information for a given account.
data InflationReward = InflationReward
  { -- | Epoch in which the reward was issued.
    epochReward :: Word64,
    -- | Slot in which the reward became effective.
    effectiveSlot :: Slot,
    -- | Amount of reward, in lamports.
    amountReward :: Lamport,
    -- | Account balance after the reward was applied.
    postBalance :: Lamport,
    -- | Vote account commission when the reward was credited.
    commissionIR :: Maybe Word8
  }
  deriving (Show, Generic)

instance FromJSON InflationReward where
  parseJSON :: Value -> Parser InflationReward
  parseJSON = withObject "InflationReward" $ \v ->
    InflationReward
      <$> v .: "epoch"
      <*> v .: "effectiveSlot"
      <*> v .: "amount"
      <*> v .: "postBalance"
      <*> v .: "commission"

------------------------------------------------------------------------------------------------

-- * getMinimumBalanceForRentExemption

------------------------------------------------------------------------------------------------

-- | Returns the minimum lamport balance required for a rent-exempt account.
--
-- Takes the account's data size in bytes and returns the required balance.
getMinimumBalanceForRentExemption :: (JsonRpc m) => Int -> m Lamport
getMinimumBalanceForRentExemption = do
  remote "getMinimumBalanceForRentExemption"
{-# INLINE getMinimumBalanceForRentExemption #-}

------------------------------------------------------------------------------------------------

-- * getStakeMinimumDelegation

------------------------------------------------------------------------------------------------

-- | Returns the minimum delegation amount required to stake.
-- Returns @RpcResponse Lamport@ with value field set to @Lamport@.
getStakeMinimumDelegation' :: (JsonRpc m) => m (RPCResponse Lamport)
getStakeMinimumDelegation' = do
  remote "getStakeMinimumDelegation"
{-# INLINE getStakeMinimumDelegation' #-}

-- | Returns the minimum amount (in lamports) required for stake delegation.
getStakeMinimumDelegation :: (JsonRpc m) => m Lamport
getStakeMinimumDelegation = value <$> getStakeMinimumDelegation'
{-# INLINE getStakeMinimumDelegation #-}

------------------------------------------------------------------------------------------------

-- * getSupply

------------------------------------------------------------------------------------------------

-- | Returns the current token supply information.
-- Returns @RpcResponse SolanaSupply@ with value field set to @SolanaSupply@.
getSupply' :: (JsonRpc m) => m (RPCResponse SolanaSupply)
getSupply' = do
  remote "getSupply"

-- | Returns the current token supply information.
getSupply :: (JsonRpc m) => m SolanaSupply
getSupply = value <$> getSupply'

-- | Contains information about the total, circulating, and non-circulating token supply.
data SolanaSupply = SolanaSupply
  { -- | Total supply in lamports.
    total :: Lamport,
    -- | Circulating supply in lamports.
    circulating :: Lamport,
    -- | Non-circulating supply in lamports.
    nonCirculating :: Lamport,
    -- | Accounts holding non-circulating tokens.
    nonCirculatingAccounts :: [SolanaPublicKey]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getVoteAccounts

------------------------------------------------------------------------------------------------

-- | Returns vote account information for all validators in the current bank.
--
-- Includes both current and delinquent vote accounts.
getVoteAccounts :: (JsonRpc m) => m VoteAccounts
getVoteAccounts = do
  remote "getVoteAccounts"

-- | Lists current and delinquent vote accounts.
data VoteAccounts = VoteAccounts
  { -- | Currently active vote accounts.
    current :: [VoteAccountsResult],
    -- | Delinquent vote accounts.
    delinquent :: [VoteAccountsResult]
  }
  deriving (Generic, Show, FromJSON)

-- | Information about a vote account.
data VoteAccountsResult = VoteAccountsResult
  { -- | Address of the vote account.
    votePubkey :: SolanaPublicKey,
    -- | Identity of the validator node.
    nodePubkey :: SolanaPublicKey,
    -- | Stake delegated to this vote account (active in this epoch).
    activatedStake :: Word64,
    -- | Whether the vote account is active this epoch.
    epochVoteAccount :: Bool,
    -- | Commission percentage charged by the validator (0–100).
    commission :: Int,
    -- | Most recent slot voted on.
    lastVote :: Slot,
    -- | Historical epoch credits: [epoch, credits, previousCredits].
    epochCredits :: [[Word64]],
    -- | Current root slot for this vote account.
    rootSlot :: Slot
  }
  deriving (Generic, Show, FromJSON)
