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
    unsafeSolanaPublicKey,
    unsafeSolanaPublicKeyRaw,
    unsafeSolanaPrivateKey,
    unsafeSolanaPrivateKeyRaw,
    getSolanaPublicKeyRaw,
    getSolanaPrivateKeyRaw,
    getSolanaSignatureRaw,
    toBase58String,
  )
where

import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.ByteString.Base58
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Generics (Generic)

toBase58String :: S.ByteString -> String
toBase58String = show . encodeBase58 bitcoinAlphabet

------------------------------------------------------------------------------------------------

-- * SolanaSignature

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

instance ToJSON SolanaPublicKey where
  toJSON :: SolanaPublicKey -> Value
  toJSON pk = toJSON (tail . init . show $ pk)

instance FromJSON SolanaPublicKey where
  parseJSON :: Value -> Parser SolanaPublicKey
  parseJSON = withText "SolanaPublicKey" $ return . unsafeSolanaPublicKey . Text.unpack

instance FromJSONKey SolanaPublicKey where
  fromJSONKey :: FromJSONKeyFunction SolanaPublicKey
  fromJSONKey = FromJSONKeyText (unsafeSolanaPublicKey . Text.unpack)

instance ToJSONKey SolanaPublicKey where
  toJSONKey :: ToJSONKeyFunction SolanaPublicKey
  toJSONKey = toJSONKeyText (Text.pack . tail . init . show)

------------------------------------------------------------------------------------------------

-- *** SolanaPrivateKey

------------------------------------------------------------------------------------------------

newtype SolanaPrivateKey
  = SolanaPrivateKey Ed25519.SecretKey
  deriving (Eq, Ord, Generic)

instance Show SolanaPrivateKey where
  show :: SolanaPrivateKey -> String
  show (SolanaPrivateKey (Ed25519.SecretKey bs)) = toBase58String bs

------------------------------------------------------------------------------------------------

-- *** Functions

------------------------------------------------------------------------------------------------

unsafeKeyFromString :: forall f. (S.ByteString -> f) -> String -> f
unsafeKeyFromString cstr str =
  let bs = fromJust . decodeBase58 bitcoinAlphabet . fromString $ str
   in if S.length bs == 32
        then
          cstr bs
        else
          error "Invalid string length for key"

unsafeKeyFromWords :: forall f. (S.ByteString -> f) -> [Word8] -> f
unsafeKeyFromWords cstr ws = cstr (S.pack ws)

unsafeSolanaPublicKey :: String -> SolanaPublicKey
unsafeSolanaPublicKey = unsafeKeyFromString (SolanaPublicKey . Ed25519.PublicKey)

unsafeSolanaPublicKeyRaw :: [Word8] -> SolanaPublicKey
unsafeSolanaPublicKeyRaw = unsafeKeyFromWords (SolanaPublicKey . Ed25519.PublicKey)

unsafeSolanaPrivateKey :: String -> SolanaPrivateKey
unsafeSolanaPrivateKey = unsafeKeyFromString (SolanaPrivateKey . Ed25519.SecretKey)

unsafeSolanaPrivateKeyRaw :: [Word8] -> SolanaPrivateKey
unsafeSolanaPrivateKeyRaw = unsafeKeyFromWords (SolanaPrivateKey . Ed25519.SecretKey)

getSolanaPublicKeyRaw :: SolanaPublicKey -> S.ByteString
getSolanaPublicKeyRaw (SolanaPublicKey (Ed25519.PublicKey bs)) = bs

getSolanaPrivateKeyRaw :: SolanaPrivateKey -> S.ByteString
getSolanaPrivateKeyRaw (SolanaPrivateKey (Ed25519.SecretKey bs)) = bs

getSolanaSignatureRaw :: SolanaSignature -> S.ByteString
getSolanaSignatureRaw (SolanaSignature (Ed25519.Signature bs)) = bs

createSolanaKeyPair :: IO (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeyPair = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypair

createSolanaKeypairFromSeed :: S.ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeypairFromSeed bs = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypairFromSeed_ bs

toSolanaPublicKey :: SolanaPrivateKey -> SolanaPublicKey
toSolanaPublicKey (SolanaPrivateKey pv) = SolanaPublicKey $ Ed25519.toPublicKey pv

sign :: SolanaPrivateKey -> S.ByteString -> S.ByteString
sign (SolanaPrivateKey sk) = Ed25519.sign sk

verify :: SolanaPublicKey -> S.ByteString -> Bool
verify (SolanaPublicKey pk) = Ed25519.verify pk

dsign :: SolanaPrivateKey -> S.ByteString -> SolanaSignature
dsign (SolanaPrivateKey sk) bs = SolanaSignature $ Ed25519.dsign sk bs

dverify :: SolanaPublicKey -> S.ByteString -> SolanaSignature -> Bool
dverify (SolanaPublicKey pk) bs (SolanaSignature sig) = Ed25519.dverify pk bs sig
