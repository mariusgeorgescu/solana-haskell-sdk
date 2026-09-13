{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.Core.Crypto
-- Description : Ed25519 keys, signatures, and Base58\/Base64 helpers for Solana.
--
-- Wrappers around "Crypto.Sign.Ed25519" providing the core cryptographic
-- types: 'SolanaPublicKey', 'SolanaPrivateKey' and 'SolanaSignature'.
-- All three 'show' as the Base58 strings used across the Solana ecosystem.
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
    mkPublicKeyFromString,
    mkPrivateKeyFromString,
    mkPrivateKeyFromBytes,
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
import Data.Either.Extra (maybeToEither)
import Data.String (IsString, fromString)
import Data.Text qualified as Text

-- | Encode a byte string as Base58 text (Bitcoin alphabet), the encoding
-- Solana uses for addresses, signatures and hashes.
toBase58String :: BS.ByteString -> String
toBase58String = tail . init . show . encodeBase58 bitcoinAlphabet

-- | Encode a byte string as Base64 text.
toBase64String :: BS.ByteString -> String
toBase64String = tail . init . show . encodeBase64'

-- | Decode Base58 text (Bitcoin alphabet); 'Nothing' if the input contains
-- characters outside the alphabet.
fromBase58String :: String -> Maybe BS.ByteString
fromBase58String = decodeBase58 bitcoinAlphabet . fromString

-- | Decode Base64 text /leniently/: invalid characters are skipped rather
-- than reported, so this never fails but may silently accept malformed
-- input.
fromBase64String :: String -> BS.ByteString
fromBase64String = decodeBase64Lenient . fromString

--- >>> fromBase64String $ toBase64String "Sun"
--- >>> fromBase58String $ toBase58String "Sun"
-- "Sun"
-- Just "Sun"

------------------------------------------------------------------------------------------------

-- * SolanaSignature

------------------------------------------------------------------------------------------------

-- | A 64-byte detached Ed25519 signature. 'Show' and the JSON instances use
-- the Base58 rendering.
newtype SolanaSignature = SolanaSignature Ed25519.Signature
  deriving (Eq, Ord)

instance Show SolanaSignature where
  show :: SolanaSignature -> String
  show (SolanaSignature (Ed25519.Signature bs)) = toBase58String bs

instance Binary SolanaSignature where
  put :: SolanaSignature -> Put
  put (SolanaSignature (Ed25519.Signature bs)) = putByteString bs
  get :: Get SolanaSignature
  get = SolanaSignature . Ed25519.Signature <$> getByteString 64

instance ToJSON SolanaSignature where
  toJSON :: SolanaSignature -> Value
  toJSON pk = toJSON (show pk)

instance FromJSON SolanaSignature where
  parseJSON :: Value -> Parser SolanaSignature
  parseJSON = withText "SolanaSignature" $ either fail pure . mkSigFromString . Text.unpack

------------------------------------------------------------------------------------------------

-- *** SolanaPublicKey

------------------------------------------------------------------------------------------------

-- | A 32-byte Ed25519 public key: the address of an account, program or
-- signer. 'Show' and the JSON instances use the Base58 rendering; the
-- 'IsString' instance is partial (see 'unsafeSolanaPublicKey').
newtype SolanaPublicKey
  = SolanaPublicKey Ed25519.PublicKey
  deriving (Eq, Ord)

instance Show SolanaPublicKey where
  show :: SolanaPublicKey -> String
  show (SolanaPublicKey (Ed25519.PublicKey bs)) = toBase58String bs

instance IsString SolanaPublicKey where
  fromString = unsafeSolanaPublicKey

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
  parseJSON = withText "SolanaPublicKey" $ either fail pure . mkPublicKeyFromString . Text.unpack

instance FromJSONKey SolanaPublicKey where
  fromJSONKey :: FromJSONKeyFunction SolanaPublicKey
  fromJSONKey = FromJSONKeyTextParser (either fail pure . mkPublicKeyFromString . Text.unpack)

instance ToJSONKey SolanaPublicKey where
  toJSONKey :: ToJSONKeyFunction SolanaPublicKey
  toJSONKey = toJSONKeyText (Text.pack . show)

------------------------------------------------------------------------------------------------

-- *** SolanaPrivateKey

------------------------------------------------------------------------------------------------

-- | A 64-byte Ed25519 secret key (seed plus public key, NaCl layout).
-- 'Show' renders it in Base58 — avoid logging values of this type.
newtype SolanaPrivateKey
  = SolanaPrivateKey Ed25519.SecretKey
  deriving (Eq, Ord)

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

-- | Build a 'SolanaSignature' from Base58 text. Calls 'error' unless the
-- input decodes to exactly 64 bytes.
unsafeSigFromString :: String -> SolanaSignature
unsafeSigFromString = either error id . mkSigFromString

mkKeyFromString :: forall f. Int -> (BS.ByteString -> f) -> String -> Either String f
mkKeyFromString n cstr str = do
  bs <- maybeToEither "Not base58" $ fromBase58String str
  if BS.length bs == n
    then Right $ cstr bs
    else Left "Invalid string length for key"

-- | Parse Base58 text into a 'SolanaPublicKey'. 'Left' if the input is not
-- Base58 or does not decode to exactly 32 bytes.
mkPublicKeyFromString :: String -> Either String SolanaPublicKey
mkPublicKeyFromString = mkKeyFromString 32 (SolanaPublicKey . Ed25519.PublicKey)

-- | Parse Base58 text into a 'SolanaPrivateKey'. 'Left' if the input is not
-- Base58 or fails the checks of 'mkPrivateKeyFromBytes' (64 bytes whose
-- public-key half is derived from the seed).
mkPrivateKeyFromString :: String -> Either String SolanaPrivateKey
mkPrivateKeyFromString str = maybeToEither "Not base58" (fromBase58String str) >>= mkPrivateKeyFromBytes

-- | Build a 'SolanaPrivateKey' from the raw 64 bytes of a NaCl secret key
-- (32-byte seed followed by the 32-byte public key). 'Left' unless the input
-- is exactly 64 bytes and its public-key half is the one derived from the
-- seed — the checks @solana-sdk@'s @Keypair::from_bytes@ performs.
mkPrivateKeyFromBytes :: BS.ByteString -> Either String SolanaPrivateKey
mkPrivateKeyFromBytes bs
  | BS.length bs /= 64 = Left ("Invalid private key length: expected 64 bytes, got " <> show (BS.length bs))
  | otherwise = case createSolanaKeypairFromSeed (BS.take 32 bs) of
      Just (_, sk) | getSolanaPrivateKeyRaw sk == bs -> Right sk
      _ -> Left "Private key public-key half does not match the key derived from its seed"

unsafeKeyFromString :: forall f. Int -> (BS.ByteString -> f) -> String -> f
unsafeKeyFromString n cstr str = either error id $ mkKeyFromString n cstr str

unsafeKeyFromWords :: forall f. Int -> (BS.ByteString -> f) -> [Word8] -> f
unsafeKeyFromWords n cstr ws
  | length ws == n = cstr (BS.pack ws)
  | otherwise = error ("Invalid key length: expected " <> show n <> " bytes, got " <> show (length ws))

-- | Partial version of 'mkPublicKeyFromString': calls 'error' on invalid
-- input. Prefer the total variant outside of literals and tests.
unsafeSolanaPublicKey :: String -> SolanaPublicKey
unsafeSolanaPublicKey = unsafeKeyFromString 32 (SolanaPublicKey . Ed25519.PublicKey)

-- | Build a 'SolanaPublicKey' directly from raw bytes. Calls 'error' unless
-- exactly 32 bytes are supplied.
unsafeSolanaPublicKeyRaw :: [Word8] -> SolanaPublicKey
unsafeSolanaPublicKeyRaw = unsafeKeyFromWords 32 (SolanaPublicKey . Ed25519.PublicKey)

-- | Partial version of 'mkPrivateKeyFromString': calls 'error' on invalid
-- input.
unsafeSolanaPrivateKey :: String -> SolanaPrivateKey
unsafeSolanaPrivateKey = either error id . mkPrivateKeyFromString

-- | Build a 'SolanaPrivateKey' directly from the raw 64 bytes of a NaCl
-- secret key. Calls 'error' on any other length; the public-key half is not
-- checked — prefer 'mkPrivateKeyFromBytes' (or 'readSigningKeyFromFile' for
-- @solana-keygen@ files).
unsafeSolanaPrivateKeyRaw :: [Word8] -> SolanaPrivateKey
unsafeSolanaPrivateKeyRaw = unsafeKeyFromWords 64 (SolanaPrivateKey . Ed25519.SecretKey)

-- | The raw 32 bytes of a public key.
getSolanaPublicKeyRaw :: SolanaPublicKey -> BS.ByteString
getSolanaPublicKeyRaw (SolanaPublicKey (Ed25519.PublicKey bs)) = bs

-- | The raw bytes of a secret key. Handle with care.
getSolanaPrivateKeyRaw :: SolanaPrivateKey -> BS.ByteString
getSolanaPrivateKeyRaw (SolanaPrivateKey (Ed25519.SecretKey bs)) = bs

-- | The raw 64 bytes of a signature.
getSolanaSignatureRaw :: SolanaSignature -> BS.ByteString
getSolanaSignatureRaw (SolanaSignature (Ed25519.Signature bs)) = bs

-- | Generate a fresh random Ed25519 keypair.
createSolanaKeyPair :: IO (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeyPair = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypair

-- | Derive a keypair deterministically from a seed. 'Nothing' unless the
-- seed is exactly 32 bytes.
createSolanaKeypairFromSeed :: BS.ByteString -> Maybe (SolanaPublicKey, SolanaPrivateKey)
createSolanaKeypairFromSeed bs = bimap SolanaPublicKey SolanaPrivateKey <$> Ed25519.createKeypairFromSeed_ bs

-- | The public key corresponding to a private key.
toSolanaPublicKey :: SolanaPrivateKey -> SolanaPublicKey
toSolanaPublicKey (SolanaPrivateKey pv) = SolanaPublicKey $ Ed25519.toPublicKey pv

-- | Sign a message, returning the /joined/ signed message (signature
-- prepended to the message). For transaction signatures use 'dsign'.
sign :: SolanaPrivateKey -> BS.ByteString -> BS.ByteString
sign (SolanaPrivateKey sk) = Ed25519.sign sk

-- | Verify a joined signed message produced by 'sign'.
verify :: SolanaPublicKey -> BS.ByteString -> Bool
verify (SolanaPublicKey pk) = Ed25519.verify pk

-- | Produce a detached 'SolanaSignature' over a byte string. This is the
-- primitive used to sign transaction messages.
dsign :: SolanaPrivateKey -> BS.ByteString -> SolanaSignature
dsign (SolanaPrivateKey sk) bs = SolanaSignature $ Ed25519.dsign sk bs

-- | Verify a detached 'SolanaSignature' over a byte string.
dverify :: SolanaPublicKey -> BS.ByteString -> SolanaSignature -> Bool
dverify (SolanaPublicKey pk) bs (SolanaSignature sig) = Ed25519.dverify pk bs sig

----
----
----
----

-- | Read a private key from a @solana-keygen@-style keypair file: a JSON
-- array of the 64 byte values of the secret key (e.g. @[1,2,...]@). Throws
-- an 'IOError' if the file cannot be read, is not an array of exactly 64
-- integers in @0..255@, or fails the checks of 'mkPrivateKeyFromBytes'.
readSigningKeyFromFile :: FilePath -> IO SolanaPrivateKey
readSigningKeyFromFile path = do
  contents <- BS.readFile path
  -- The JSON parser's own error would quote the unparsed remainder of the
  -- file, i.e. secret bytes, so decode failures get a fixed message.
  case eitherDecodeStrict contents of
    Left _ -> failWith "not a JSON array of 64 integers in 0..255"
    Right ws -> either failWith return (mkPrivateKeyFromBytes (BS.pack ws))
  where
    failWith err = fail ("readSigningKeyFromFile: " <> path <> ": " <> err)
