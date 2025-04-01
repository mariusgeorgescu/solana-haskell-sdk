{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Tokenomics where

import Core.Account (Lamport)
import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Aeson.Types
import Data.Word
import GHC.Generics
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getInflationGovernor

------------------------------------------------------------------------------------------------

-- | Returns the current inflation governor.
getInflationGovernor :: (JsonRpc m) => m InflationGovernor
getInflationGovernor = do
  remote "getInflationGovernor"
{-# INLINE getInflationGovernor #-}

-- | Parameters defining the current inflation governor configuration..
data InflationGovernor = InflationGovernor
  { -- | Percentage of total inflation allocated to the foundation
    foundation :: Double,
    -- | Duration of foundation pool inflation in years
    foundationTerm :: Double,
    -- | Initial inflation percentage from time 0
    initial :: Double,
    -- | Rate per year at which inflation is lowered. (Rate reduction is derived using the target slot time in genesis config)
    taper :: Double,
    -- |  Terminal inflation percentage
    terminal :: Double
  }
  deriving (Show, Generic, FromJSON)

------------------------------------------------------------------------------------------------

-- * getInflationRate

------------------------------------------------------------------------------------------------

-- | Returns the specific inflation values for the current epoch.
getInflationRate :: (JsonRpc m) => m InflationRate
getInflationRate = do
  remote "getInflationRate"
{-# INLINE getInflationRate #-}

-- | Contains the inflation values for the given epoch.
data InflationRate = InflationRate
  { -- | Total inflation.
    totalInflation :: Double,
    -- | Inflation allocated to validators.
    validatorInflation :: Double,
    -- | Inflation allocated to the foundation.
    foundationInflation :: Double,
    -- | Epoch for which these values are valid.
    epochInflation :: Double
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

-- | Returns the inflation / staking reward for a list of addresses for an epoch.
getInflationReward :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe InflationReward]
getInflationReward = do
  remote "getInflationReward"
{-# INLINE getInflationReward #-}

-- | Contains the inflation / staking reward for a list of addresses for an epoch
data InflationReward = InflationReward
  { -- | Epoch for which reward occurred.
    epochReward :: Word64,
    -- | The slot in which the rewards are effective.
    effectiveSlot :: Slot,
    -- | Reward amount in lamports.
    amountReward :: Lamport,
    -- | Post balance of the account in lamports.
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

-- | Returns the minimum lamport balance required for a rent-exempt account, based on its data size..
--  Receives the Account's data length and returns the balance balance in 'Lamport'.
getMinimumBalanceForRentExemption :: (JsonRpc m) => Int -> m Lamport
getMinimumBalanceForRentExemption = do
  remote "getMinimumBalanceForRentExemption"
{-# INLINE getMinimumBalanceForRentExemption #-}

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

-- * getSupply

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

-- | Contains information about the solana token supply.
data SolanaSupply = SolanaSupply
  { -- | Total supply in lamports
    total :: Lamport,
    -- | Circulating supply in lamports.
    circulating :: Lamport,
    -- | Non-circulating supply in lamports.
    nonCirculating :: Lamport,
    -- | An array of account addresses of non-circulating accounts.
    nonCirculatingAccounts :: [SolanaPublicKey]
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getVoteAccounts

------------------------------------------------------------------------------------------------

-- | Returns the account info and associated stake for all the voting accounts in the current bank.
getVoteAccounts :: (JsonRpc m) => m VoteAccounts
getVoteAccounts = do
  remote "getVoteAccounts"

data VoteAccounts = VoteAccounts
  { current :: [VoteAccountsResult],
    delinquent :: [VoteAccountsResult]
  }
  deriving (Generic, Show, FromJSON)

data VoteAccountsResult = VoteAccountsResult
  { -- | Vote account address
    votePubkey :: SolanaPublicKey,
    -- | Validator identity
    nodePubkey :: SolanaPublicKey,
    -- | The stake, in lamports, delegated to this vote account and active in this epoch
    activatedStake :: Word64,
    -- | Whether the vote account is staked for this epoch
    epochVoteAccount :: Bool,
    -- | Percentage (0-100) of rewards payout owed to the vote account
    commission :: Int,
    -- | Most recent slot voted on by this vote account
    lastVote :: Slot,
    -- | Latest history of earned credits for up to five epochs, as an array of arrays containing: [epoch, credits, previousCredits]
    epochCredits :: [[Word64]],
    -- | Current root slot for this vote account
    rootSlot :: Slot
  }
  deriving (Generic, Show, FromJSON)