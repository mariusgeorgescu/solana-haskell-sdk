{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.Core.Account where

import Data.Aeson.Types
import Data.ByteString qualified as S
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Base (Alternative (..))
import GHC.Generics (Generic)
import Network.Solana.Constants
import Network.Solana.Core.Crypto
import Text.Printf (printf)

-- | Lamport is the smallest unit of SOL (1 SOL = 1 billion lamports).
newtype Lamport = Lamport
  { unLamport :: Word64
  }
  deriving (Eq, Ord, Generic, Enum)
  deriving newtype (Num, Real, Integral)

instance ToJSON Lamport where
  toJSON :: Lamport -> Value
  toJSON (Lamport amnt) = toJSON amnt

instance FromJSON Lamport where
  parseJSON :: Value -> Parser Lamport
  parseJSON value = do
    v <- parseJSON @Word64 value
    return $ Lamport v

instance Show Lamport where
  show :: Lamport -> String
  show (Lamport n) =
    let (sol :: Double) = fromIntegral n / fromIntegral lamportsPerSol
     in printf "%.9f SOL ◎" sol

------------------------------------------------------------------------------------------------

-- ** Account

------------------------------------------------------------------------------------------------

data Account = Account
  { pubkey :: SolanaPublicKey,
    account :: AccountInfo
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- ** Account Info

------------------------------------------------------------------------------------------------

{- Solana Account structure.
    Accounts have a max size of 10MiB and every account on Solana has the same base Account type.
-}
data AccountInfo = AccountInfo
  { -- | Amount of lamports in the account
    lamports :: Lamport,
    {-  For non-executable accounts, this generally stores state that is meant to be read-only.
        For program accounts (smart contracts), this contains the executable program code.
        The data field is commonly referred to as "account data".
    -}
    dataField :: AccountData,
    {- The  program ID  'SolanaPublicKey' associated with the program that owns this account.
       Only the owner program can modify the account's data or deduct its lamports balance.
    -}
    owner :: SolanaPublicKey,
    -- |   A boolean flag that indicates whether this account contains a loaded program.
    executable :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AccountInfo where
  parseJSON :: Value -> Parser AccountInfo
  parseJSON = do
    withObject "AccountInfo" $
      \v ->
        AccountInfo
          <$> v .: "lamports"
          <*> v .: "data"
          <*> v .: "owner"
          <*> v .: "executable"

------------------------------------------------------------------------------------------------

-- ** Account Data

------------------------------------------------------------------------------------------------

-- | A byte array that stores arbitrary data for an account.
data AccountData
  = AccountDataBinary
      { accData :: S.ByteString
      }
  | AccountDataJSON {accDataObj :: String}
  deriving (Eq, Generic)

instance Show AccountData where
  show :: AccountData -> String
  show (AccountDataBinary bs) = toBase58String bs
  show (AccountDataJSON o) = show o

instance ToJSON AccountData where
  toJSON :: AccountData -> Value
  toJSON ac = toJSON (show ac)

instance FromJSON AccountData where
  parseJSON :: Value -> Parser AccountData
  parseJSON v =
    let base64StringParser =
          withText
            "AccountDataText"
            (return . AccountDataBinary . fromBase64String . T.unpack)

        base58StringParser =
          withText
            "AccountDataText"
            (return . AccountDataBinary . fromJust . fromBase58String . T.unpack)

        encodedParser =
          withArray
            "AccountDataArray"
            ( \arr -> do
                case V.last arr of
                  "base58" -> base58StringParser $ V.head arr
                  "base64" -> base64StringParser $ V.head arr
                  "json" -> withText "AccountDataJSON" (return . AccountDataJSON . T.unpack) $ V.head arr
                  "jsonParsed" -> withText "AccountDataJSON" (return . AccountDataJSON . T.unpack) $ V.head arr
                  _ -> error "unsupported encoding"
            )
            v
     in base64StringParser v <|> encodedParser
