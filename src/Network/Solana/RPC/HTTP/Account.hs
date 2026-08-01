{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.RPC.HTTP.Account
-- Description : Solana RPC methods for retrieving account-related data.
--
-- This module provides bindings to various Solana JSON-RPC methods
-- for fetching account information, balances, transaction signatures,
-- and related metadata.
module Network.Solana.RPC.HTTP.Account where

import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Account (Account, AccountInfo, Lamport)
import Network.Solana.Core.Crypto (SolanaPublicKey, SolanaSignature)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- ** getAccountInfo

------------------------------------------------------------------------------------------------

-- | Fetches all available data for the given account address, using the given configuration.
-- Returns 'RPCResponse' ('Maybe' 'AccountInfo'), where 'Nothing' indicates the account does not exist.
getAccountInfo' :: (JsonRpc m) => SolanaPublicKey -> ConfigurationObject -> m (RPCResponse (Maybe AccountInfo))
getAccountInfo' = do
  remote "getAccountInfo"
{-# INLINE getAccountInfo' #-}

-- | Fetches all available data for the given account address, requesting
-- base64 encoding (the node's default base58 encoding rejects account data
-- over 128 bytes). Returns 'Nothing' if the account does not exist.
getAccountInfo :: (JsonRpc m) => SolanaPublicKey -> m (Maybe AccountInfo)
getAccountInfo pubKey = value <$> getAccountInfo' pubKey cfgJustEncodingBase64
{-# INLINE getAccountInfo #-}

------------------------------------------------------------------------------------------------

-- ** getBalance

------------------------------------------------------------------------------------------------

-- | Returns the balance, in lamports, of the specified account.
-- Returns 'RPCResponse' 'Lamport'.
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

-- | Returns account information for a list of addresses, using the given configuration.
-- For any missing account, the result contains 'Nothing' in its place.
getMultipleAccounts' :: (JsonRpc m) => [SolanaPublicKey] -> ConfigurationObject -> m (RPCResponse [Maybe AccountInfo])
getMultipleAccounts' = do
  remote "getMultipleAccounts"
{-# INLINE getMultipleAccounts' #-}

-- | Returns account information for a list of addresses, requesting base64
-- encoding (the node's default base58 encoding rejects account data over
-- 128 bytes). For any missing account, the result contains 'Nothing' in its place.
getMultipleAccounts :: (JsonRpc m) => [SolanaPublicKey] -> m [Maybe AccountInfo]
getMultipleAccounts pubKeys = value <$> getMultipleAccounts' pubKeys cfgJustEncodingBase64
{-# INLINE getMultipleAccounts #-}

------------------------------------------------------------------------------------------------

-- * getProgramAccounts

------------------------------------------------------------------------------------------------

-- | Returns all accounts owned by the specified program address, using the given configuration.
getProgramAccounts' :: (JsonRpc m) => SolanaPublicKey -> ConfigurationObject -> m [Account]
getProgramAccounts' = do
  remote "getProgramAccounts"
{-# INLINE getProgramAccounts' #-}

-- | Returns all accounts owned by the specified program address, requesting
-- base64 encoding (the node's default base58 encoding rejects account data
-- over 128 bytes).
getProgramAccounts :: (JsonRpc m) => SolanaPublicKey -> m [Account]
getProgramAccounts pk = getProgramAccounts' pk cfgJustEncodingBase64
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
    -- | Error details if the transaction failed ('Data.Aeson.Value' as
    -- returned by the RPC), 'Nothing' if it succeeded.
    err :: Maybe Value,
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
    -- | Error details if the transaction failed ('Data.Aeson.Value' as
    -- returned by the RPC), 'Nothing' if it succeeded.
    errTxStatus :: Maybe Value,
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
