{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Crypto (tests) where

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
        decode (encode sig) @?= sig
    ]
