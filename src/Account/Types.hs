{-# LANGUAGE OverloadedStrings #-}

module Account.Types where

import Data.ByteString qualified as B
import Data.ByteString.Base58
import Data.Word
import GHC.Generics

-- | Every account on Solana is identifiable by a unique 32 byte address
newtype PubKey = PubKey B.ByteString
  deriving (Eq, Generic)

instance Show PubKey where
  show :: PubKey -> String
  show (PubKey bs) = show $ encodeBase58 (bitcoinAlphabet) bs

type Lamports = Word64

-- | Define the Account structure in Haskell
data Account = Account
  { -- | Amount of lamports in the account
    lamports :: Word64,
    -- |  Data held in this account (as a ByteString for efficiency)
    dataField :: B.ByteString,
    -- |  The program that owns this account
    owner :: PubKey,
    -- |  Whether this account contains a loaded program
    executable :: Bool
  }
  deriving (Show, Eq, Generic)
