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
    unsafeSolanaPublicKey,
  )
where

import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.ByteString.Base58
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

toBase58String :: S.ByteString -> String
toBase58String = show . encodeBase58 (bitcoinAlphabet)

s :: S.ByteString
s = "AddressLookupTab1e1111111111111111111111111"

--- >>> S.length <$>  decodeBase58 (bitcoinAlphabet) s
-- Just 32

secretKeyFromBS :: S.ByteString -> Either String SolanaPublicKey
secretKeyFromBS bs =
  let result = decodeBase58 (bitcoinAlphabet) bs
   in case result of
        Nothing -> Left "failed base58 decoding"
        Just pk ->
          if S.length bs == 32
            then
              Right (SolanaPublicKey (Ed25519.PublicKey pk))
            else Left "invalid length"

unsafeSolanaPublicKey :: S.ByteString -> SolanaPublicKey
unsafeSolanaPublicKey = SolanaPublicKey . Ed25519.PublicKey . fromJust . decodeBase58 (bitcoinAlphabet)

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
  put (SolanaSignature (Ed25519.Signature bs)) = putByteString bs
  get :: Get SolanaSignature
  get = SolanaSignature . Ed25519.Signature <$> getByteString 32

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
  put (SolanaPublicKey (Ed25519.PublicKey bs)) = putByteString bs
  get :: Get SolanaPublicKey
  get = SolanaPublicKey . Ed25519.PublicKey <$> getByteString 32

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
