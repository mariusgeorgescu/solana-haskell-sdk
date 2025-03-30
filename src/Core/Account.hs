{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Account where

import Constants
import Core.Crypto
import Data.Aeson.Types
import Data.ByteString qualified as S
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | Lamport is the smallest unit of SOL (1 SOL = 1 billion lamports).
newtype Lamport = Lamport
  { unLamport :: Word64
  }
  deriving (Eq, Ord, Generic)
  deriving newtype (Num)

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
    let n'int = fromIntegral n
        (sol :: Double) = fromIntegral n / fromIntegral lamportsPerSol
     in if n'int > lamportsPerSol
          then printf "%.9f SOL" sol
          else printf "% lamports" n

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

-- ** Instruction Data

------------------------------------------------------------------------------------------------

-- | A byte array that stores arbitrary data for an account.
newtype AccountData = AccountData
  { accData :: S.ByteString
  }
  deriving (Eq, Generic)

instance Show AccountData where
  show :: AccountData -> String
  show (AccountData bs) = toBase58String bs

instance ToJSON AccountData where
  toJSON :: AccountData -> Value
  toJSON ac = toJSON (show ac)

instance FromJSON AccountData where
  parseJSON :: Value -> Parser AccountData
  parseJSON =
    withText "AccountData" (return . AccountData . encodeUtf8)