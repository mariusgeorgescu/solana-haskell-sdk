{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Vote program. Instruction data is bincode-encoded
-- (u32 little-endian discriminant), like the System and Stake programs.
-- Covered discriminants: 0, 1, 3, 4, 5, 7 (account-management surface); the
-- consensus @Vote@ instruction (2) and the remaining vote-state variants
-- (6, 8-15) are not modeled.
module Network.Solana.NativePrograms.Vote where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Sysvar qualified as Sysvar

-- | Vote program address.
voteProgramId :: SolanaPublicKey
voteProgramId = "Vote111111111111111111111111111111111111111"

-- | Arguments to initialize a vote account.
data VoteInit = VoteInit
  { viNodePubkey :: SolanaPublicKey,
    viAuthorizedVoter :: SolanaPublicKey,
    viAuthorizedWithdrawer :: SolanaPublicKey,
    viCommission :: Word8
  }
  deriving (Eq, Show, Generic)

instance Binary VoteInit where
  put :: VoteInit -> Put
  put (VoteInit node voter withdrawer commission) = do
    putByteString (getSolanaPublicKeyRaw node)
    putByteString (getSolanaPublicKeyRaw voter)
    putByteString (getSolanaPublicKeyRaw withdrawer)
    putWord8 commission
  get :: Get VoteInit
  get = VoteInit <$> get <*> get <*> get <*> getWord8

-- | Which vote authority an operation concerns (bincode u32: 0 voter, 1 withdrawer).
data VoteAuthorize = AuthorizeVoter | AuthorizeVoteWithdrawer
  deriving (Eq, Show, Enum, Bounded, Generic)

putVoteAuthorize :: VoteAuthorize -> Put
putVoteAuthorize va = putWord32le (fromIntegral (fromEnum va))

getVoteAuthorize :: Get VoteAuthorize
getVoteAuthorize = do
  v <- getWord32le
  if v <= 1
    then pure (toEnum (fromIntegral v))
    else fail ("VoteAuthorize: invalid value " <> show v)

-- | Vote program instructions (covered subset).
data VoteInstruction
  = InitializeAccount VoteInit
  | VoteAuthorizeIx SolanaPublicKey VoteAuthorize
  | VoteWithdraw Word64
  | UpdateValidatorIdentity
  | UpdateCommission Word8
  | VoteAuthorizeChecked VoteAuthorize
  deriving (Eq, Show, Generic)

instance Binary VoteInstruction where
  put :: VoteInstruction -> Put
  put (InitializeAccount voteInit) = do
    putWord32le 0
    put voteInit
  put (VoteAuthorizeIx newAuthority voteAuthorize) = do
    putWord32le 1
    putByteString (getSolanaPublicKeyRaw newAuthority)
    putVoteAuthorize voteAuthorize
  put (VoteWithdraw amount) = do
    putWord32le 3
    putWord64le amount
  put UpdateValidatorIdentity = putWord32le 4
  put (UpdateCommission commission) = do
    putWord32le 5
    putWord8 commission
  put (VoteAuthorizeChecked voteAuthorize) = do
    putWord32le 7
    putVoteAuthorize voteAuthorize

  get :: Get VoteInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> InitializeAccount <$> get
      1 -> VoteAuthorizeIx <$> get <*> getVoteAuthorize
      3 -> VoteWithdraw <$> getWord64le
      4 -> pure UpdateValidatorIdentity
      5 -> UpdateCommission <$> getWord8
      7 -> VoteAuthorizeChecked <$> getVoteAuthorize
      _ -> fail ("VoteInstruction: unknown discriminant " <> show disc)

-- | Creates instruction to "Initialize a vote account"
-- Receives the new vote account and the initialization arguments (node
-- identity, authorized voter/withdrawer and commission).
-- # Account references
-- 0. `[WRITE]` Uninitialized vote account
-- 1. `[]` Rent sysvar
-- 2. `[]` Clock sysvar
-- 3. `[SIGNER]` New validator identity (node)
initializeVoteAccount :: SolanaPublicKey -> VoteInit -> Instruction
initializeVoteAccount voteAccount voteInit =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = viNodePubkey voteInit, isSigner = True, isWritable = False}
    ]
    (InitializeAccount voteInit)

-- | Creates instruction to "Authorize a key to send votes or issue a withdrawal"
-- Receives the vote account, the current authority, the new authority and
-- which authority is being changed.
-- # Account references
-- 0. `[WRITE]` Vote account to be updated
-- 1. `[]` Clock sysvar
-- 2. `[SIGNER]` The vote or withdraw authority
authorizeVote :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> VoteAuthorize -> Instruction
authorizeVote voteAccount currentAuthority newAuthority voteAuthorize =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False}
    ]
    (VoteAuthorizeIx newAuthority voteAuthorize)

-- | Creates instruction to "Withdraw lamports from a vote account"
-- Receives the vote account, the recipient account, the withdraw authority
-- and the number of lamports to withdraw.
-- # Account references
-- 0. `[WRITE]` Vote account to withdraw from
-- 1. `[WRITE]` Recipient account
-- 2. `[SIGNER]` Withdraw authority
withdrawVote :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Word64 -> Instruction
withdrawVote voteAccount recipient withdrawAuthority amount =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = recipient, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = withdrawAuthority, isSigner = True, isWritable = False}
    ]
    (VoteWithdraw amount)

-- | Creates instruction to "Update the vote account's validator identity (node pubkey)"
-- Receives the vote account, the new validator identity (which must sign) and
-- the withdraw authority.
-- # Account references
-- 0. `[WRITE]` Vote account to be updated
-- 1. `[SIGNER]` New validator identity (node)
-- 2. `[SIGNER]` Withdraw authority
updateValidatorIdentity :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
updateValidatorIdentity voteAccount newNode withdrawAuthority =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = newNode, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = withdrawAuthority, isSigner = True, isWritable = False}
    ]
    UpdateValidatorIdentity

-- | Creates instruction to "Update the vote account's commission"
-- Receives the vote account, the withdraw authority and the new commission.
-- # Account references
-- 0. `[WRITE]` Vote account to be updated
-- 1. `[SIGNER]` Withdraw authority
updateCommission :: SolanaPublicKey -> SolanaPublicKey -> Word8 -> Instruction
updateCommission voteAccount withdrawAuthority commission =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = withdrawAuthority, isSigner = True, isWritable = False}
    ]
    (UpdateCommission commission)

-- | Creates instruction to "Authorize a key to send votes or issue a withdrawal", checking
-- that the new authority signs (a safer version of 'authorizeVote').
-- Receives the vote account, the current authority, the new authority (which
-- must sign) and which authority is being changed.
-- # Account references
-- 0. `[WRITE]` Vote account to be updated
-- 1. `[]` Clock sysvar
-- 2. `[SIGNER]` The vote or withdraw authority
-- 3. `[SIGNER]` The new vote or withdraw authority
authorizeVoteChecked :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> VoteAuthorize -> Instruction
authorizeVoteChecked voteAccount currentAuthority newAuthority voteAuthorize =
  mkInstruction
    voteProgramId
    [ AccountMeta {accountPubKey = voteAccount, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = Sysvar.clock, isSigner = False, isWritable = False},
      AccountMeta {accountPubKey = currentAuthority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = newAuthority, isSigner = True, isWritable = False}
    ]
    (VoteAuthorizeChecked voteAuthorize)
