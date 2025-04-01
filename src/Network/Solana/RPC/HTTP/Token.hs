{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.RPC.HTTP.Token where

import Data.Aeson
import Data.Word
import GHC.Generics (Generic)
import Network.JsonRpc.TinyClient (JsonRpc (..))
import Network.Solana.Core.Account (Account)
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.RPC.HTTP.Types

------------------------------------------------------------------------------------------------

-- * getTokenAccountBalance

------------------------------------------------------------------------------------------------

-- | Returns the token balance of an SPL Token account.
-- Returns 'RpcResponse AmmountObject' with detailed balance information.
getTokenAccountBalance' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse AmmountObject)
getTokenAccountBalance' = do
  remote "getTokenAccountBalance"

-- | Returns the token balance of an SPL Token account.
getTokenAccountBalance :: (JsonRpc m) => SolanaPublicKey -> m AmmountObject
getTokenAccountBalance = fmap value . getTokenAccountBalance'

-- | Contains SPL token balance details.
data AmmountObject = AmmountObject
  { -- | The raw balance without decimals, a string representation of .
    amount :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals :: Word8,
    -- | The balance using mint-prescribed decimals (deprecated).
    uiAmount :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString :: String
  }
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON)

------------------------------------------------------------------------------------------------

-- * getTokenAccountsByDelegate

------------------------------------------------------------------------------------------------

-- | Returns all SPL Token accounts delegated to the provided delegate address.
getTokenAccountsByDelegate :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> ConfigurationObject -> m (RPCResponse [Account])
getTokenAccountsByDelegate = do
  remote "getTokenAccountsByDelegate"
{-# INLINE getTokenAccountsByDelegate #-}

------------------------------------------------------------------------------------------------

-- * getTokenAccountsByOwner

------------------------------------------------------------------------------------------------

-- | Specifies either a mint or a program to filter token accounts.
data SolanaPubKeyWithPurpose = Mint SolanaPublicKey | Program SolanaPublicKey
  deriving (Generic)

instance ToJSON SolanaPubKeyWithPurpose where
  toJSON (Mint key) = object ["mint" .= key]
  toJSON (Program key) = object ["programId" .= key]

-- | Returns all SPL Token accounts owned by the specified wallet.
getTokenAccountsByOwner' :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> ConfigurationObject -> m (RPCResponse [Account])
getTokenAccountsByOwner' = do
  remote "getTokenAccountsByOwner"
{-# INLINE getTokenAccountsByOwner' #-}

-- | Returns all SPL Token accounts owned by the specified wallet.
getTokenAccountsByOwner :: (JsonRpc m) => SolanaPublicKey -> SolanaPubKeyWithPurpose -> m [Account]
getTokenAccountsByOwner pk pkwv = value <$> getTokenAccountsByOwner' pk pkwv cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwner #-}

-- | Returns all SPL Token accounts for the specified owner and mint address.
getTokenAccountsByOwnerAndMint :: (JsonRpc m) => SolanaPublicKey -> SolanaPublicKey -> m [Account]
getTokenAccountsByOwnerAndMint pk pkwv = value <$> getTokenAccountsByOwner' pk (Mint pkwv) cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwnerAndMint #-}

-- | Returns all SPL Token accounts for the specified owner and program ID.
getTokenAccountsByOwnerAndProgram :: (JsonRpc m) => SolanaPublicKey -> SolanaPublicKey -> m [Account]
getTokenAccountsByOwnerAndProgram pk pkwv = value <$> getTokenAccountsByOwner' pk (Program pkwv) cfgJustEncodingBase64
{-# INLINE getTokenAccountsByOwnerAndProgram #-}

------------------------------------------------------------------------------------------------

-- * getTokenLargestAccounts

------------------------------------------------------------------------------------------------

-- | Returns the largest accounts for a given SPL Token mint, sorted by balance.
-- Returns 'RpcResponse [AmmountObjectWithAddr]'.
getTokenLargestAccounts' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse [AmmountObjectWithAddr])
getTokenLargestAccounts' = do
  remote "getTokenLargestAccounts"

-- | Returns the largest accounts for a given SPL Token mint, sorted by balance.
getTokenLargestAccounts :: (JsonRpc m) => SolanaPublicKey -> m [AmmountObjectWithAddr]
getTokenLargestAccounts = fmap value . getTokenLargestAccounts'

-- | Contains address and balance information for an SPL Token account.
data AmmountObjectWithAddr = AmmountObjectWithAddr
  { -- | The account address.
    address' :: SolanaPublicKey,
    -- | The raw balance without decimals, a string representation of 'Word64'.
    amount' :: String,
    -- | Number of base 10 digits to the right of the decimal place.
    decimals' :: Word8,
    -- | The balance using mint-prescribed decimals (deprecated).
    uiAmount' :: Maybe Double,
    -- | The balance as a string, using mint-prescribed decimals.
    uiAmountString' :: String
  }
  deriving (Generic, Show)

instance FromJSON AmmountObjectWithAddr where
  parseJSON = withObject "AmmountObjectWithAddr" $ \v ->
    AmmountObjectWithAddr
      <$> v .: "address"
      <*> v .: "amount"
      <*> v .: "decimals"
      <*> v .: "uiAmount"
      <*> v .: "uiAmountString"

------------------------------------------------------------------------------------------------

-- * getTokenSupply

------------------------------------------------------------------------------------------------

-- | Returns the total supply of an SPL Token.
-- Returns 'RpcResponse AmmountObject' with the supply balance.
getTokenSupply' :: (JsonRpc m) => SolanaPublicKey -> m (RPCResponse AmmountObject)
getTokenSupply' = do
  remote "getTokenSupply"

-- | Returns the total supply of an SPL Token.
getTokenSupply :: (JsonRpc m) => SolanaPublicKey -> m AmmountObject
getTokenSupply = fmap value . getTokenSupply'
