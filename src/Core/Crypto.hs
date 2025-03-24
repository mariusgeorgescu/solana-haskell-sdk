{-# LANGUAGE OverloadedStrings #-}

module Core.Crypto
  ( createSolanaKeyPair,
    createSolanaKeypairFromSeed,
    toSolanaPublicKey,
    sign,
    verify,
    dsign,
    dverify,
    SolanaPublicKey,
    SolanaPrivateKey,
    SolanaSignature,
    toBase58String,
  )
where

import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary
import Data.ByteString qualified as S
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import GHC.Generics (Generic)

toBase58String :: S.ByteString -> String
toBase58String = show . encodeBase58 (bitcoinAlphabet)

------------------------------------------------------------------------------------------------

-- *** SolanaSignature

------------------------------------------------------------------------------------------------

newtype SolanaSignature = SolanaSignature Ed25519.Signature
  deriving (Eq, Ord, Generic)

instance Show SolanaSignature where
  show :: SolanaSignature -> String
  show (SolanaSignature (Ed25519.Signature bs)) = toBase58String bs

instance Binary SolanaSignature where
  put :: SolanaSignature -> Put
  put (SolanaSignature (Ed25519.Signature bs)) = put bs
  get :: Get SolanaSignature
  get = SolanaSignature . Ed25519.Signature <$> get

------------------------------------------------------------------------------------------------

-- *** SolanaPublicKey

------------------------------------------------------------------------------------------------

newtype SolanaPublicKey
  = SolanaPublicKey Ed25519.PublicKey
  deriving (Eq, Ord, Generic)

instance Show SolanaPublicKey where
  show :: SolanaPublicKey -> String
  show (SolanaPublicKey (Ed25519.PublicKey bs)) = toBase58String bs

instance Binary SolanaPublicKey where
  put :: SolanaPublicKey -> Put
  put (SolanaPublicKey (Ed25519.PublicKey bs)) = put bs
  get :: Get SolanaPublicKey
  get = SolanaPublicKey . Ed25519.PublicKey <$> get

------------------------------------------------------------------------------------------------

-- *** SolanaPrivateKey

------------------------------------------------------------------------------------------------

newtype SolanaPrivateKey
  = SolanaPrivateKey Ed25519.SecretKey
  deriving (Eq, Ord, Generic)

instance Show SolanaPrivateKey where
  show :: SolanaPrivateKey -> String
  show (SolanaPrivateKey (Ed25519.SecretKey bs)) = show $ encodeBase58 (bitcoinAlphabet) bs

------------------------------------------------------------------------------------------------

-- *** Functions

------------------------------------------------------------------------------------------------

createSolanaKeyPair :: IO (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeyPair = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypair

createSolanaKeypairFromSeed :: S.ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeypairFromSeed bs = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypairFromSeed_ bs

toSolanaPublicKey :: SolanaPrivateKey -> SolanaPublicKey
toSolanaPublicKey (SolanaPrivateKey pv) = SolanaPublicKey $ Ed25519.toPublicKey pv

sign :: SolanaPrivateKey -> S.ByteString -> S.ByteString
sign (SolanaPrivateKey sk) bs = Ed25519.sign sk bs

verify :: SolanaPublicKey -> S.ByteString -> Bool
verify (SolanaPublicKey pk) bs = Ed25519.verify pk bs

dsign :: SolanaPrivateKey -> S.ByteString -> SolanaSignature
dsign (SolanaPrivateKey sk) bs = SolanaSignature $ Ed25519.dsign sk bs

dverify :: SolanaPublicKey -> S.ByteString -> SolanaSignature -> Bool
dverify (SolanaPublicKey pk) bs (SolanaSignature sig) = Ed25519.dverify pk bs sig
