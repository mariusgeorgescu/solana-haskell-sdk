{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Crypto (tests) where

import Data.Aeson (eitherDecode)
import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

newtype Bytes32 = Bytes32 BS.ByteString
  deriving (Show)

instance Arbitrary Bytes32 where
  arbitrary = Bytes32 . BS.pack <$> vectorOf 32 arbitrary

tests :: TestTree
tests =
  testGroup
    "crypto"
    [ testProperty "base58 round-trip" $ \(Bytes32 bs) ->
        fromBase58String (toBase58String bs) === Just bs,
      testProperty "base64 round-trip" $ \(Bytes32 bs) ->
        fromBase64String (toBase64String bs) === bs,
      testProperty "public key Binary round-trip" $ \(Bytes32 bs) ->
        let pk = unsafeSolanaPublicKeyRaw (BS.unpack bs)
         in decode (encode pk) === pk,
      testCase "signature Binary round-trip (64 bytes)" $ do
        Just (_, priv) <- pure (createSolanaKeypairFromSeed (BS.replicate 32 1))
        let sig = dsign priv "solana-haskell-sdk"
        decode (encode sig) @?= sig,
      testCase "private key Base58 round-trip (64-byte NaCl secret key)" $ do
        (_, priv) <- createSolanaKeyPair
        case mkPrivateKeyFromString (show priv) of
          Left err -> assertFailure ("mkPrivateKeyFromString failed on a genuine keypair: " <> err)
          Right priv' -> do
            show priv' @?= show priv
            dsign priv' "solana-haskell-sdk" @?= dsign priv "solana-haskell-sdk",
      testCase "SolanaPublicKey FromJSON rejects invalid base58" $
        case eitherDecode "\"0OIl\"" :: Either String SolanaPublicKey of
          Left _ -> pure ()
          Right pk -> assertFailure ("expected parse failure, got " <> show pk),
      testCase "SolanaSignature FromJSON rejects invalid base58" $
        case eitherDecode "\"0OIl\"" :: Either String SolanaSignature of
          Left _ -> pure ()
          Right sig -> assertFailure ("expected parse failure, got " <> show sig)
    ]
