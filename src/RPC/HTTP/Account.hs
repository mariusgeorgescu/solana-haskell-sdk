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

-- | Fetches all available data for the given account address.
-- Returns 'RpcResponse (Maybe AccountInfo)', where 'Nothing' indicates the account does not exist.
getAccountInfo' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse (Maybe AccountInfo))
getAccountInfo' = do
  remote "getAccountInfo"
{-# INLINE getAccountInfo' #-}

-- | Fetches all available data for the given account address.
-- Returns 'Nothing' if the account does not exist.
getAccountInfo :: (JsonRpc m) => SolanaPublicKey -> m (Maybe AccountInfo)
getAccountInfo pubKey = value <$> getAccountInfo' pubKey
{-# INLINE getAccountInfo #-}

------------------------------------------------------------------------------------------------

-- ** getBalance

------------------------------------------------------------------------------------------------

-- | Returns the balance, in lamports, of the specified account.
-- Returns 'RpcResponse Lamport'.
getBalance' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse Lamport)
getBalance' = do
  remote "getBalance"
{-# INLINE getBalance' #-}

-- | Returns the balance, in lamports, of the specified account.
getBalance :: (JsonRpc m) => SolanaPublicKey -> m Lamport
getBalance pubKey = value <$> getBalance' pubKey
{-# INLINE getBalance #-}

------------------------------------------------------------------------------------------------

-- * getMultipleAccounts

------------------------------------------------------------------------------------------------

-- | Returns account information for a list of addresses.
-- For any missing account, the result contains 'Nothing' in its place.
getMultipleAccounts' :: (JsonRpc m) => [SolanaPublicKey] -> m (RPCResponse [Maybe AccountInfo])
getMultipleAccounts' = do
  remote "getMultipleAccounts"
{-# INLINE getMultipleAccounts' #-}

-- | Returns account information for a list of addresses.
-- For any missing account, the result contains 'Nothing' in its place.
getMultipleAccounts :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe AccountInfo]
getMultipleAccounts = fmap value . getMultipleAccounts'
{-# INLINE getMultipleAccounts #-}

------------------------------------------------------------------------------------------------

-- * getProgramAccounts

------------------------------------------------------------------------------------------------

-- | Returns all accounts owned by the specified program address.
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

-- | Returns up to the 20 accounts with the highest balances in lamports.
-- Results may be cached for up to two hours.
getLargestAccounts' :: (JsonRpc m) => m (RPCResponse [AddressAndLamports])
getLargestAccounts' = do
  remote "getLargestAccounts"
{-# INLINE getLargestAccounts' #-}

-- | Returns up to the 20 accounts with the highest balances in lamports.
-- Results may be cached for up to two hours.
getLargestAccounts :: (JsonRpc m) => m [(SolanaPublicKey, Lamport)]
getLargestAccounts = fmap (liftA2 (,) address lamports) . value <$> getLargestAccounts'
{-# INLINE getLargestAccounts #-}

------------------------------------------------------------------------------------------------

-- * getSignaturesForAddress

------------------------------------------------------------------------------------------------

-- | Returns confirmed transaction signatures involving the given address, in reverse chronological order.
getSignaturesForAddress :: (JsonRpc m) => SolanaPublicKey -> m [TransactionSignatureInformation]
getSignaturesForAddress = do
  remote "getSignaturesForAddress"
{-# INLINE getSignaturesForAddress #-}

-- | Metadata for a confirmed transaction signature involving a given address.
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

-- | Returns the confirmation status and slot info for the specified transaction signatures.
-- Each signature should be the transaction’s first signature (used as a unique identifier).
getSignatureStatuses' :: (JsonRpc m) => [SolanaSignature] -> ConfigurationObject -> m (RPCResponse [Maybe TransactionSignatureStatus])
getSignatureStatuses' = do
  remote "getSignatureStatuses"
{-# INLINE getSignatureStatuses' #-}

-- | Returns the confirmation status and slot info for the specified transaction signatures.
-- Each signature should be the transaction’s first signature (used as a unique identifier).
getSignatureStatuses :: (JsonRpc m) => [SolanaSignature] -> m [Maybe TransactionSignatureStatus]
getSignatureStatuses sigs = value <$> getSignatureStatuses' sigs (defaultConfigObject {searchTransactionHistory = Just True})
{-# INLINE getSignatureStatuses #-}

-- | Status metadata for a given transaction signature.
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
