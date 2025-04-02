{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.Sysvar where

import Network.Solana.Core.Crypto (SolanaPublicKey)

-- | The Clock sysvar contains data on cluster time, including the current slot, epoch, and
-- estimated wall-clock Unix timestamp. It is updated every slot.
clock :: SolanaPublicKey
clock = "SysvarC1ock11111111111111111111111111111111"

-- | The EpochSchedule sysvar contains epoch scheduling constants that are set in genesis,
-- and enables calculating the number of slots in a given epoch, the epoch for a given slot, etc.
epochSchedule :: SolanaPublicKey
epochSchedule = "SysvarEpochSchedu1e111111111111111111111111"

-- | The Fees sysvar contains the fee calculator for the current slot.
-- It is updated every slot, based on the fee-rate governor.
fees :: SolanaPublicKey
fees = "SysvarFees111111111111111111111111111111111"

-- | The Instructions sysvar contains the serialized instructions in a Message while that Message is being processed.
-- This allows program instructions to reference other instructions in the same transaction.
instructions :: SolanaPublicKey
instructions = "Sysvar1nstructions1111111111111111111111111"

-- | The RecentBlockhashes sysvar contains the active recent blockhashes as well as their associated fee calculators.
-- It is updated every slot.
recentBlockhashes :: SolanaPublicKey
recentBlockhashes = "SysvarRecentB1ockHashes11111111111111111111"

-- | The Rent sysvar contains the rental rate.
rent :: SolanaPublicKey
rent = "SysvarRent111111111111111111111111111111111"

-- | The SlotHashes sysvar contains the most recent hashes of the slot's parent banks. It is updated every slot.
slotHashes :: SolanaPublicKey
slotHashes = "SysvarS1otHashes111111111111111111111111111"

-- | The SlotHistory sysvar contains a bitvector of slots present over the last epoch. It is updated every slot.
slotHistory :: SolanaPublicKey
slotHistory = "SysvarS1otHistory11111111111111111111111111"

-- | The StakeHistory sysvar contains the history of cluster-wide stake activations and de-activations per epoch.
-- It is updated at the start of every epoch.
stakeHistory :: SolanaPublicKey
stakeHistory = "SysvarStakeHistory1111111111111111111111111"

-- | The EpochRewards sysvar holds a record of epoch rewards distribution in Solana, including block rewards and staking rewards.
epochRewards :: SolanaPublicKey
epochRewards = "SysvarEpochRewards1111111111111111111111111"

-- | The LastRestartSlot sysvar contains the slot number of the last restart or 0 (zero) if none ever happened.
lastRestartSlot :: SolanaPublicKey
lastRestartSlot = "SysvarLastRestartS1ot1111111111111111111111"