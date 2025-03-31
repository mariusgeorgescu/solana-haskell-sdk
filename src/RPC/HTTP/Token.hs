{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module RPC.HTTP.Token where

import Core.Account (Account)
import Core.Crypto (SolanaPublicKey)
import Data.Aeson
import Data.Aeson.Types
import Data.Word
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import RPC.HTTP.Types

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

-- | Contains information about amount.
data AmmountObject = AmmountObject
  { -- | The raw balance without decimals, a string representation of u64
    amount :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals :: Word8,
    -- | The balance, using mint-prescribed decimals DEPRECATED.
    uiAmount :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString :: String
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON)

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

data SolanaPubKeyWithPurpose = Mint SolanaPublicKey | Program SolanaPublicKey
  deriving (Generic)

instance ToJSON SolanaPubKeyWithPurpose where
  toJSON :: SolanaPubKeyWithPurpose -> Value
  toJSON (Mint key) = object ["mint" .= key]
  toJSON (Program key) = object ["programId" .= key]

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

-- | Contains information about SPL Token account.
data AmmountObjectWithAddr = AmmountObjectWithAddr
  { -- | The raw balance without decimals, a string representation of u64
    address' :: SolanaPublicKey,
    -- | The raw balance without decimals, a string representation of u64
    amount' :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals' :: Word8,
    -- | The balance, using mint-prescribed decimals DEPRECATED.
    uiAmount' :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString' :: String
  }
  deriving (Generic, Show)

instance FromJSON AmmountObjectWithAddr where
  parseJSON :: Value -> Parser AmmountObjectWithAddr
  parseJSON = withObject "AmmountObjectWithAddr" $ \v ->
    AmmountObjectWithAddr
      <$> v .: "address"
      <*> v .: "amount"
      <*> v .: "decimals"
      <*> v .: "uiAmount"
      <*> v .: "uiAmountString"

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
