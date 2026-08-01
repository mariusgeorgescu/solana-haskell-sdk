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
