{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.Core.Crypto
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
    unsafeSigFromString,
    getSolanaPublicKeyRaw,
    getSolanaPrivateKeyRaw,
    getSolanaSignatureRaw,
    toBase58String,
    toBase64String,
    fromBase64String,
    fromBase58String,
    readSigningKeyFromFile,
  )
where

import Crypto.Sign.Ed25519 qualified as Ed25519
import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base58
import Data.ByteString.Base64 (decodeBase64Lenient, encodeBase64')
import Data.ByteString.Char8 qualified as BS8
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text qualified as Text
import GHC.Generics (Generic)

toBase58String :: BS.ByteString -> String
toBase58String = tail . init . show . encodeBase58 bitcoinAlphabet

toBase64String :: BS.ByteString -> String
toBase64String = tail . init . show . encodeBase64'

fromBase58String :: String -> Maybe BS.ByteString
fromBase58String = decodeBase58 bitcoinAlphabet . fromString

fromBase64String :: String -> BS.ByteString
fromBase64String = decodeBase64Lenient . fromString

--- >>> fromBase64String $ toBase64String "Sun"
--- >>> fromBase58String $ toBase58String "Sun"
-- "Sun"
-- Just "Sun"

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

instance ToJSON SolanaSignature where
  toJSON :: SolanaSignature -> Value
  toJSON pk = toJSON (show pk)

instance FromJSON SolanaSignature where
  parseJSON :: Value -> Parser SolanaSignature
  parseJSON = withText "SolanaSignature" $ return . unsafeSigFromString . Text.unpack

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
  toJSON pk = toJSON (show pk)

instance FromJSON SolanaPublicKey where
  parseJSON :: Value -> Parser SolanaPublicKey
  parseJSON = withText "SolanaPublicKey" $ return . unsafeSolanaPublicKey . Text.unpack

instance FromJSONKey SolanaPublicKey where
  fromJSONKey :: FromJSONKeyFunction SolanaPublicKey
  fromJSONKey = FromJSONKeyText (unsafeSolanaPublicKey . Text.unpack)

instance ToJSONKey SolanaPublicKey where
  toJSONKey :: ToJSONKeyFunction SolanaPublicKey
  toJSONKey = toJSONKeyText (Text.pack . show)

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

mkSigFromString :: String -> Either String SolanaSignature
mkSigFromString str = do
  bs <- maybeToEither "Not base58" $ fromBase58String str
  if BS.length bs == 64
    then Right $ (SolanaSignature . Ed25519.Signature) bs
    else Left "Invalid string length for sig"

unsafeSigFromString :: String -> SolanaSignature
unsafeSigFromString = either error id . mkSigFromString

mkKeyromString :: forall f. (BS.ByteString -> f) -> String -> Either String f
mkKeyromString cstr str = do
  bs <- maybeToEither "Not base58" $ fromBase58String str
  if BS.length bs == 32
    then Right $ cstr bs
    else Left "Invalid string length for key"

mkPublicKeyFromString :: String -> Either String SolanaPublicKey
mkPublicKeyFromString = mkKeyromString (SolanaPublicKey . Ed25519.PublicKey)

mkPrivateKeyFromString :: String -> Either String SolanaPrivateKey
mkPrivateKeyFromString = mkKeyromString (SolanaPrivateKey . Ed25519.SecretKey)

unsafeKeyFromString :: forall f. (BS.ByteString -> f) -> String -> f
unsafeKeyFromString cstr str = either error id $ mkKeyromString cstr str

unsafeKeyFromWords :: forall f. (BS.ByteString -> f) -> [Word8] -> f
unsafeKeyFromWords cstr ws = cstr (BS.pack ws)

unsafeSolanaPublicKey :: String -> SolanaPublicKey
unsafeSolanaPublicKey = unsafeKeyFromString (SolanaPublicKey . Ed25519.PublicKey)

unsafeSolanaPublicKeyRaw :: [Word8] -> SolanaPublicKey
unsafeSolanaPublicKeyRaw = unsafeKeyFromWords (SolanaPublicKey . Ed25519.PublicKey)

unsafeSolanaPrivateKey :: String -> SolanaPrivateKey
unsafeSolanaPrivateKey = unsafeKeyFromString (SolanaPrivateKey . Ed25519.SecretKey)

unsafeSolanaPrivateKeyRaw :: [Word8] -> SolanaPrivateKey
unsafeSolanaPrivateKeyRaw = unsafeKeyFromWords (SolanaPrivateKey . Ed25519.SecretKey)

getSolanaPublicKeyRaw :: SolanaPublicKey -> BS.ByteString
getSolanaPublicKeyRaw (SolanaPublicKey (Ed25519.PublicKey bs)) = bs

getSolanaPrivateKeyRaw :: SolanaPrivateKey -> BS.ByteString
getSolanaPrivateKeyRaw (SolanaPrivateKey (Ed25519.SecretKey bs)) = bs

getSolanaSignatureRaw :: SolanaSignature -> BS.ByteString
getSolanaSignatureRaw (SolanaSignature (Ed25519.Signature bs)) = bs

createSolanaKeyPair :: IO (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeyPair = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypair

createSolanaKeypairFromSeed :: BS.ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeypairFromSeed bs = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypairFromSeed_ bs

toSolanaPublicKey :: SolanaPrivateKey -> SolanaPublicKey
toSolanaPublicKey (SolanaPrivateKey pv) = SolanaPublicKey $ Ed25519.toPublicKey pv

sign :: SolanaPrivateKey -> BS.ByteString -> BS.ByteString
sign (SolanaPrivateKey sk) = Ed25519.sign sk

verify :: SolanaPublicKey -> BS.ByteString -> Bool
verify (SolanaPublicKey pk) = Ed25519.verify pk

dsign :: SolanaPrivateKey -> BS.ByteString -> SolanaSignature
dsign (SolanaPrivateKey sk) bs = SolanaSignature $ Ed25519.dsign sk bs

dverify :: SolanaPublicKey -> BS.ByteString -> SolanaSignature -> Bool
dverify (SolanaPublicKey pk) bs (SolanaSignature sig) = Ed25519.dverify pk bs sig

----
----
----
----

readSigningKeyFromFile :: FilePath -> IO SolanaPrivateKey
readSigningKeyFromFile path = do
  contents <- BS8.readFile path
  let word8List = read (BS8.unpack contents) :: [Word8]
  return $ unsafeSolanaPrivateKeyRaw word8List

-- -- TODO

-- getKeyPairFromFile
-- writeKeys
-- Write priv key to file
-- writeKeyToFileBase68 privKey

-- Write priv key to file
-- writeKeyToFileRaw privKey
