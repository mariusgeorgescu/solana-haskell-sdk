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
    Ed25519.Signature (..),
    toBase58String,
  )
where

import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString qualified as S
import Data.ByteString.Base58 (bitcoinAlphabet, encodeBase58)
import GHC.Generics (Generic)

toBase58String :: S.ByteString -> String
toBase58String = show . encodeBase58 (bitcoinAlphabet)

newtype SolanaPublicKey
  = SolanaPublicKey Ed25519.PublicKey
  deriving (Eq, Ord, Generic)

instance Show SolanaPublicKey where
  show :: SolanaPublicKey -> String
  show (SolanaPublicKey (Ed25519.PublicKey bs)) = toBase58String bs

newtype SolanaPrivateKey
  = SolanaPrivateKey Ed25519.SecretKey
  deriving (Eq, Ord, Generic)

instance Show SolanaPrivateKey where
  show :: SolanaPrivateKey -> String
  show (SolanaPrivateKey (Ed25519.SecretKey bs)) = show $ encodeBase58 (bitcoinAlphabet) bs

createSolanaKeyPair :: IO (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeyPair = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypair

createSolanaKeypairFromSeed :: S.ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeypairFromSeed bs = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypairFromSeed_ bs

toSolanaPublicKey :: SolanaPrivateKey -> SolanaPublicKey
toSolanaPublicKey (SolanaPrivateKey pv) = SolanaPublicKey $ Ed25519.toPublicKey pv

sign :: SolanaPrivateKey -> S.ByteString -> S.ByteString
sign (SolanaPrivateKey sk) bs = Ed25519.sign sk bs

verify :: SolanaPublicKey -> S.ByteString -> Bool
verify (SolanaPublicKey sk) bs = Ed25519.verify sk bs

dsign :: SolanaPrivateKey -> S.ByteString -> Ed25519.Signature
dsign (SolanaPrivateKey sk) bs = Ed25519.dsign sk bs

dverify :: SolanaPublicKey -> S.ByteString -> Ed25519.Signature -> Bool
dverify (SolanaPublicKey sk) sig bs = Ed25519.dverify sk sig bs
