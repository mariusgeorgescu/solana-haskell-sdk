{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.Core.Account
-- Description : Accounts, account state, and the 'Lamport' unit.
module Network.Solana.Core.Account where

import Data.Aeson.Types
import Data.ByteString qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Solana.Constants
import Network.Solana.Core.Crypto
import Text.Printf (printf)

-- | Lamport is the smallest unit of SOL (1 SOL = 1 billion lamports).
-- 'Show' renders the amount converted to SOL (e.g. @0.000000001 SOL ◎@), not the raw count.
newtype Lamport = Lamport
  { -- | The raw lamport amount.
    unLamport :: Word64
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

-- | An account paired with its address, as returned by RPC methods such as
-- @getProgramAccounts@.
data Account = Account
  { -- | The account's address.
    pubkey :: SolanaPublicKey,
    -- | The account's state.
    account :: AccountInfo
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- ** Account Info

------------------------------------------------------------------------------------------------

-- | The base state shared by every Solana account. Accounts hold at most
-- 10 MiB of data.
data AccountInfo = AccountInfo
  { -- | Amount of lamports in the account
    lamports :: Lamport,
    -- | The account's data: executable code for program accounts,
    -- program-defined state otherwise. Named @dataField@ because @data@ is
    -- reserved in Haskell; serialized as @data@ in JSON.
    dataField :: AccountData,
    -- | The program that owns this account. Only the owner program may
    -- modify the account's data or debit its lamports.
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
--
-- On the wire, RPC account @data@ is either a bare Base58 string (the node's
-- default @binary@ encoding, only offered for accounts of at most 128 bytes)
-- or a @[data, encoding]@ pair; 'toJSON' emits the bare Base58 form.
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
            ( maybe (fail "AccountData: invalid base58") (pure . AccountDataBinary)
                . fromBase58String
                . T.unpack
            )

        encodedParser =
          withArray
            "AccountDataArray"
            ( \arr ->
                if V.length arr /= 2
                  then fail "AccountData: expected [data, encoding] pair"
                  else case arr V.! 1 of
                    "base58" -> base58StringParser $ arr V.! 0
                    "base64" -> base64StringParser $ arr V.! 0
                    "json" -> withText "AccountDataJSON" (return . AccountDataJSON . T.unpack) $ arr V.! 0
                    "jsonParsed" -> withText "AccountDataJSON" (return . AccountDataJSON . T.unpack) $ arr V.! 0
                    other -> fail ("AccountData: unsupported encoding: " <> show other)
            )
            v
     in case v of
          String _ -> base58StringParser v
          _ -> encodedParser
