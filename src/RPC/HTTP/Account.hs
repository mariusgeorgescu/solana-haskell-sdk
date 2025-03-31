{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Account where

import Core.Account (Account, AccountInfo, Lamport)
import Core.Crypto (SolanaPublicKey, SolanaSignature)
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

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

-- * getLargestAccounts

------------------------------------------------------------------------------------------------

-- | Contains the address and value of an account.
data AddressAndLamports
  = AddressAndLamports
  { -- | Account address
    address :: SolanaPublicKey,
    -- | Number of lamports in the account
    lamports :: Lamport
  }
  deriving (Generic, Show, FromJSON)

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

-- * getSignaturesForAddress

------------------------------------------------------------------------------------------------

-- | Returns signatures for confirmed transactions that include the given address in their 'accountKeys' list.
-- Returns signatures backwards in time from the provided signature or most recent confirmed block
getSignaturesForAddress :: (JsonRpc m) => SolanaPublicKey -> m [TransactionSignatureInformation]
getSignaturesForAddress = do
  remote "getSignaturesForAddress"
{-# INLINE getSignaturesForAddress #-}

data TransactionSignatureInformation = TransactionSignatureInformation
  { -- | Transaction signature
    signature :: SolanaSignature,
    -- | The slot that contains the block with the transaction
    slotTxSig :: Slot,
    -- | Error if transaction failed, 'Nothing' if transaction succeeded.
    err :: Maybe String,
    -- | Memo associated with the transaction, 'Nothing' if no memo is present
    memo :: Maybe String,
    -- | Estimated production time, as Unix timestamp (seconds since the Unix epoch) of when transaction was processed. 'Nothing' if not available.
    blockTime :: Maybe Int64,
    -- | The transaction's cluster confirmation status;
    confirmationStatus :: Maybe String
  }
  deriving (Show)

instance FromJSON TransactionSignatureInformation where
  parseJSON :: Value -> Parser TransactionSignatureInformation
  parseJSON = withObject "TransactionSignatureInformation" $ \v ->
    TransactionSignatureInformation
      <$> v .: "signature"
      <*> v .: "slot"
      <*> v .: "err"
      <*> v .: "memo"
      <*> v .: "blockTime"
      <*> v .: "confirmationStatus"

------------------------------------------------------------------------------------------------

-- * getSignatureStatuses

------------------------------------------------------------------------------------------------

-- | Returns the statuses of a list of @TransactionSignatureStatus@.
-- Each signature must be a TxId (the first signature in a transaction, which can be used to uniquely identify
-- the transaction across the complete ledger).
getSignatureStatuses' :: (JsonRpc m) => [SolanaSignature] -> ConfigurationObject -> m (RPCResponse [Maybe TransactionSignatureStatus])
getSignatureStatuses' = do
  remote "getSignatureStatuses"
{-# INLINE getSignatureStatuses' #-}

-- | Returns the statuses of a list of @TransactionSignatureStatus@.
-- Each signature must be a TxId (the first signature in a transaction, which can be used to uniquely identify
-- the transaction across the complete ledger).
getSignatureStatuses :: (JsonRpc m) => [SolanaSignature] -> m [Maybe TransactionSignatureStatus]
getSignatureStatuses sigs = value <$> getSignatureStatuses' sigs (defaultConfigObject {searchTransactionHistory = Just True})
{-# INLINE getSignatureStatuses #-}

data TransactionSignatureStatus = TransactionSignatureStatus
  { -- | The slot the transaction was processed
    slotTxStatus :: Slot,
    -- | Number of blocks since signature confirmation, 'Nothing' if rooted, as well as finalized by a supermajority of the cluster.
    confirmationsTxStatus :: Maybe Int,
    -- | Error if transaction failed, 'Nothing' if transaction succeeded.
    errTxStatus :: Maybe String,
    -- | The transaction's cluster confirmation status.
    confirmationStatusTxStatus :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON TransactionSignatureStatus where
  parseJSON :: Value -> Parser TransactionSignatureStatus
  parseJSON = withObject "TransactionSignatureStatus" $ \v ->
    TransactionSignatureStatus
      <$> v .: "slot"
      <*> v .: "confirmations"
      <*> v .: "err"
      <*> v .: "confirmationStatus"
