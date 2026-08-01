{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Stake program. Instruction data is bincode-encoded
-- (u32 little-endian discriminant), like the System Program.
-- Covered discriminants: 0-7, 9, 10, 13; the seed-authority variants (8, 11),
-- SetLockupChecked (12), DeactivateDelinquent (14) and the deprecated
-- Redelegate (15) are not modeled.
module Network.Solana.NativePrograms.Stake where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Sysvar qualified as Sysvar

-- | Stake program address.
stakeProgramId :: SolanaPublicKey
stakeProgramId = "Stake11111111111111111111111111111111111111"

-- | Stake config account address (legacy; still passed to DelegateStake).
stakeConfigId :: SolanaPublicKey
stakeConfigId = "StakeConfig11111111111111111111111111111111"

-- | The authorities of a stake account.
data Authorized = Authorized
  { aStaker :: SolanaPublicKey,
    aWithdrawer :: SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

instance Binary Authorized where
  put :: Authorized -> Put
  put (Authorized staker withdrawer) = do
    putByteString (getSolanaPublicKeyRaw staker)
    putByteString (getSolanaPublicKeyRaw withdrawer)
  get :: Get Authorized
  get = Authorized <$> get <*> get

-- | Lockup constraints on a stake account.
data Lockup = Lockup
  { lUnixTimestamp :: Int64,
    lEpoch :: Word64,
    lCustodian :: SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

instance Binary Lockup where
  put :: Lockup -> Put
  put (Lockup ts epoch cust) = do
    putInt64le ts
    putWord64le epoch
    putByteString (getSolanaPublicKeyRaw cust)
  get :: Get Lockup
  get = Lockup <$> getInt64le <*> getWord64le <*> get

-- | Which stake authority an operation concerns (bincode u32: 0 staker, 1 withdrawer).
data StakeAuthorize = AuthorizeStaker | AuthorizeWithdrawer
  deriving (Eq, Show, Enum, Bounded, Generic)

putStakeAuthorize :: StakeAuthorize -> Put
putStakeAuthorize sa = putWord32le (fromIntegral (fromEnum sa))

getStakeAuthorize :: Get StakeAuthorize
getStakeAuthorize = do
  v <- getWord32le
  if v <= 1
    then pure (toEnum (fromIntegral v))
    else fail ("StakeAuthorize: invalid value " <> show v)

-- | Partial lockup update; bincode Option fields (u8 tag + value).
data LockupArgs = LockupArgs
  { laUnixTimestamp :: Maybe Int64,
    laEpoch :: Maybe Word64,
    laCustodian :: Maybe SolanaPublicKey
  }
  deriving (Eq, Show, Generic)

-- | bincode Option<T>: u8 tag 0 (None) | 1 followed by T.
putBincodeOption :: (a -> Put) -> Maybe a -> Put
putBincodeOption _ Nothing = putWord8 0
putBincodeOption p (Just x) = putWord8 1 >> p x

getBincodeOption :: Get a -> Get (Maybe a)
getBincodeOption g = do
  tag <- getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> g
    _ -> fail ("bincode Option: invalid tag " <> show tag)

instance Binary LockupArgs where
  put :: LockupArgs -> Put
  put (LockupArgs ts epoch cust) = do
    putBincodeOption putInt64le ts
    putBincodeOption putWord64le epoch
    putBincodeOption (putByteString . getSolanaPublicKeyRaw) cust
  get :: Get LockupArgs
  get =
    LockupArgs
      <$> getBincodeOption getInt64le
      <*> getBincodeOption getWord64le
      <*> getBincodeOption get

-- | Stake program instructions (covered subset).
data StakeInstruction
  = Initialize Authorized Lockup
  | Authorize SolanaPublicKey StakeAuthorize
  | DelegateStake
  | Split Word64
  | Withdraw Word64
  | Deactivate
  | SetLockup LockupArgs
  | Merge
  | InitializeChecked
  | AuthorizeChecked StakeAuthorize
  | GetMinimumDelegation
  deriving (Eq, Show, Generic)

instance Binary StakeInstruction where
  put :: StakeInstruction -> Put
  put (Initialize authorized lockup) = do
    putWord32le 0
    put authorized
    put lockup
  put (Authorize newAuthority stakeAuthorize) = do
    putWord32le 1
    putByteString (getSolanaPublicKeyRaw newAuthority)
    putStakeAuthorize stakeAuthorize
  put DelegateStake = putWord32le 2
  put (Split amount) = do
    putWord32le 3
    putWord64le amount
  put (Withdraw amount) = do
    putWord32le 4
    putWord64le amount
  put Deactivate = putWord32le 5
  put (SetLockup args) = do
    putWord32le 6
    put args
  put Merge = putWord32le 7
  put InitializeChecked = putWord32le 9
  put (AuthorizeChecked stakeAuthorize) = do
    putWord32le 10
    putStakeAuthorize stakeAuthorize
  put GetMinimumDelegation = putWord32le 13

  get :: Get StakeInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> Initialize <$> get <*> get
      1 -> Authorize <$> get <*> getStakeAuthorize
      2 -> pure DelegateStake
      3 -> Split <$> getWord64le
      4 -> Withdraw <$> getWord64le
      5 -> pure Deactivate
      6 -> SetLockup <$> get
      7 -> pure Merge
      9 -> pure InitializeChecked
      10 -> AuthorizeChecked <$> getStakeAuthorize
      13 -> pure GetMinimumDelegation
      _ -> fail ("StakeInstruction: unknown discriminant " <> show disc)

-- | Creates instruction to "Initialize a stake with lockup and authorization information"
-- Receives the new stake account, the authorized staker/withdrawer and the lockup.
-- # Account references
-- 0. `[WRITE]` Uninitialized stake account
-- 1. `[]` Rent sysvar
initialize :: SolanaPublicKey -> Authorized -> Lockup -> Instruction
initialize stakeAccount authorized lockup =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False}
    ]
    (Initialize authorized lockup)

-- | Creates instruction to "Authorize a key to manage stake or withdrawal"
-- Receives the stake account, the current stake or withdraw authority, the new authority,
-- which authority is being changed and, if updating the withdrawer before lockup expiration,
-- the lockup custodian.
-- # Account references
-- 0. `[WRITE]` Stake account to be updated
-- 1. `[]` Clock sysvar
-- 2. `[SIGNER]` The stake or withdraw authority
-- 3. `[SIGNER]` (optional) Lockup authority, if updating StakeAuthorize::Withdrawer before lockup expiration
authorize :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> StakeAuthorize -> Maybe SolanaPublicKey -> Instruction
authorize stakeAccount currentAuthority newAuthority stakeAuthorize custodian =
  mkInstruction
    stakeProgramId
    ( [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False}
      ]
        ++ maybe [] (\c -> [AccountMeta {accountPubKey = c, isSigner = True, isWritable = False}]) custodian
    )
    (Authorize newAuthority stakeAuthorize)

-- | Creates instruction to "Delegate a stake to a particular vote account"
-- Receives the stake account, the authorized staker and the vote account.
-- # Account references
-- 0. `[WRITE]` Initialized stake account to be delegated
-- 1. `[]` Vote account to which this stake will be delegated
-- 2. `[]` Clock sysvar
-- 3. `[]` Stake history sysvar
-- 4. `[]` Address of config account that carries stake config
-- 5. `[SIGNER]` Stake authority
delegateStake :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
delegateStake stakeAccount stakeAuthority voteAccount =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.stakeHistory, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = stakeConfigId, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = stakeAuthority, isSigner = True, isWritable = False}
    ]
    DelegateStake

-- | Creates instruction to "Split u64 tokens and stake off a stake account into another stake account"
-- Receives the stake account to split from, the uninitialized stake account that will take the
-- split-off amount, the stake authority and the number of lamports to split.
-- # Account references
-- 0. `[WRITE]` Stake account to be split; must be in the Initialized or Stake state
-- 1. `[WRITE]` Uninitialized stake account that will take the split-off amount
-- 2. `[SIGNER]` Stake authority
split :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction
split stakeAccount splitStakeAccount stakeAuthority amount =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = splitStakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = stakeAuthority, isSigner = True, isWritable = False}
    ]
    (Split amount)

-- | Creates instruction to "Withdraw unstaked lamports from the stake account"
-- Receives the stake account, the recipient account, the withdraw authority, the number of
-- lamports to withdraw and, if withdrawing before lockup expiration, the lockup custodian.
-- # Account references
-- 0. `[WRITE]` Stake account from which to withdraw
-- 1. `[WRITE]` Recipient account
-- 2. `[]` Clock sysvar
-- 3. `[]` Stake history sysvar
-- 4. `[SIGNER]` Withdraw authority
-- 5. `[SIGNER]` (optional) Lockup authority, if before lockup expiration
withdraw :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Maybe SolanaPublicKey -> Instruction
withdraw stakeAccount recipient withdrawAuthority amount custodian =
  mkInstruction
    stakeProgramId
    ( [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = recipient, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = Sysvar.stakeHistory, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = withdrawAuthority, isSigner = True, isWritable = False}
      ]
        ++ maybe [] (\c -> [AccountMeta {accountPubKey = c, isSigner = True, isWritable = False}]) custodian
    )
    (Withdraw amount)

-- | Creates instruction to "Deactivate the stake in the account"
-- Receives the stake account and the stake authority.
-- # Account references
-- 0. `[WRITE]` Delegated stake account
-- 1. `[]` Clock sysvar
-- 2. `[SIGNER]` Stake authority
deactivate :: SolanaPublicKey -> SolanaPublicKey -> Instruction
deactivate stakeAccount stakeAuthority =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = stakeAuthority, isSigner = True, isWritable = False}
    ]
    Deactivate

-- | Creates instruction to "Set stake lockup"
-- Receives the stake account, the lockup fields to update, and the lockup custodian
-- (or, if no lockup is currently in force, the withdraw authority).
-- # Account references
-- 0. `[WRITE]` Stake account
-- 1. `[SIGNER]` Lockup authority or withdraw authority
setLockup :: SolanaPublicKey -> LockupArgs -> SolanaPublicKey -> Instruction
setLockup stakeAccount lockupArgs custodian =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = custodian, isSigner = True, isWritable = False}
    ]
    (SetLockup lockupArgs)

-- | Creates instruction to "Merge two stake accounts"
-- Receives the destination stake account, the source stake account (which is merged into the
-- destination and deactivated) and the stake authority.
-- # Account references
-- 0. `[WRITE]` Destination stake account for the merge
-- 1. `[WRITE]` Source stake account for the merge
-- 2. `[]` Clock sysvar
-- 3. `[]` Stake history sysvar
-- 4. `[SIGNER]` Stake authority
merge :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
merge destinationStakeAccount sourceStakeAccount stakeAuthority =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = destinationStakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = sourceStakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.stakeHistory, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = stakeAuthority, isSigner = True, isWritable = False}
    ]
    Merge

-- | Creates instruction to "Initialize a stake with authorization information", checking that the
-- withdrawer signs (a safer version of 'initialize').
-- Receives the new stake account and the authorized staker/withdrawer; the withdrawer must sign.
-- # Account references
-- 0. `[WRITE]` Uninitialized stake account
-- 1. `[]` Rent sysvar
-- 2. `[]` The stake authority
-- 3. `[SIGNER]` The withdraw authority
initializeChecked :: SolanaPublicKey -> Authorized -> Instruction
initializeChecked stakeAccount authorized =
  mkInstruction
    stakeProgramId
    [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = aStaker authorized, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = aWithdrawer authorized, isSigner = True, isWritable = False}
    ]
    InitializeChecked

-- | Creates instruction to "Authorize a key to manage stake or withdrawal", checking that the new
-- authority signs (a safer version of 'authorize').
-- Receives the stake account, the current stake or withdraw authority, the new authority (which
-- must sign), which authority is being changed and, if updating the withdrawer before lockup
-- expiration, the lockup custodian.
-- # Account references
-- 0. `[WRITE]` Stake account to be updated
-- 1. `[]` Clock sysvar
-- 2. `[SIGNER]` The stake or withdraw authority
-- 3. `[SIGNER]` The new stake or withdraw authority
-- 4. `[SIGNER]` (optional) Lockup authority, if updating StakeAuthorize::Withdrawer before lockup expiration
authorizeChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> StakeAuthorize -> Maybe SolanaPublicKey -> Instruction
authorizeChecked stakeAccount currentAuthority newAuthority stakeAuthorize custodian =
  mkInstruction
    stakeProgramId
    ( [ AccountMeta {accountPubKey = stakeAccount, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False},
        AccountMeta {accountPubKey = newAuthority, isSigner = True, isWritable = False}
      ]
        ++ maybe [] (\c -> [AccountMeta {accountPubKey = c, isSigner = True, isWritable = False}]) custodian
    )
    (AuthorizeChecked stakeAuthorize)
