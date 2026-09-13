{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Crypto (tests) where

import Control.Exception (ErrorCall, IOException, evaluate, try)
import Data.Aeson (eitherDecode)
import Data.Binary (decode, encode)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Network.Solana.Core.Crypto
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

newtype Bytes32 = Bytes32 BS.ByteString
  deriving (Show)

instance Arbitrary Bytes32 where
  arbitrary = Bytes32 . BS.pack <$> vectorOf 32 arbitrary

-- | The keypair derived from a seed of 32 one-bytes; its 64 secret-key bytes
-- are what @test/fixtures/keypair.json@ holds in @solana-keygen@ format.
fixtureKeypair :: (SolanaPublicKey, SolanaPrivateKey)
fixtureKeypair = case createSolanaKeypairFromSeed (BS.replicate 32 1) of
  Just kp -> kp
  Nothing -> error "fixtureKeypair: 32-byte seed rejected"

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
          Right sig -> assertFailure ("expected parse failure, got " <> show sig),
      testGroup
        "mkPrivateKeyFromBytes"
        [ testCase "accepts a genuine 64-byte secret key" $ do
            let (pk, sk) = fixtureKeypair
            mkPrivateKeyFromBytes (getSolanaPrivateKeyRaw sk) @?= Right sk
            fmap toSolanaPublicKey (mkPrivateKeyFromBytes (getSolanaPrivateKeyRaw sk)) @?= Right pk,
          testCase "rejects empty, truncated, seed-only and over-long input" $
            mapM_
              (\bs -> assertBool ("accepted " <> show (BS.length bs) <> " bytes") (isLeft (mkPrivateKeyFromBytes bs)))
              [BS.empty, BS.pack [1], BS.replicate 32 1, BS.replicate 63 1, BS.replicate 65 1],
          testCase "rejects a mismatched public-key half" $ do
            let raw = getSolanaPrivateKeyRaw (snd fixtureKeypair)
                corrupted = BS.init raw <> BS.singleton (BS.last raw + 1)
            assertBool
              "accepted a key whose public half is not derived from its seed"
              (isLeft (mkPrivateKeyFromBytes corrupted)),
          testProperty "rejects every length other than 64" $
            forAll (choose (0, 130) `suchThat` (/= 64)) $ \n ->
              forAll (vectorOf n arbitrary) $ \ws -> isLeft (mkPrivateKeyFromBytes (BS.pack ws)),
          testProperty "rejects random 64-byte input" $
            forAll (vectorOf 64 arbitrary) $ \ws -> isLeft (mkPrivateKeyFromBytes (BS.pack ws)),
          testCase "mkPrivateKeyFromString applies the same checks to Base58 input" $ do
            let raw = getSolanaPrivateKeyRaw (snd fixtureKeypair)
                corrupted = BS.take 32 raw <> BS.replicate 32 9
            mkPrivateKeyFromString (toBase58String raw) @?= Right (snd fixtureKeypair)
            assertBool
              "accepted a Base58 key whose public half is not derived from its seed"
              (isLeft (mkPrivateKeyFromString (toBase58String corrupted)))
        ],
      testGroup
        "readSigningKeyFromFile"
        [ testCase "loads a solana-keygen keypair file" $ do
            sk <- readSigningKeyFromFile "test/fixtures/keypair.json"
            sk @?= snd fixtureKeypair
            show (toSolanaPublicKey sk) @?= "AKnL4NNf3DGWZJS6cPknBuEGnVsV4A4m5tgebLHaRSZ9"
            dverify (toSolanaPublicKey sk) "solana-haskell-sdk" (dsign sk "solana-haskell-sdk") @?= True,
          testCase "rejects a truncated key file" $ do
            r <- try (readSigningKeyFromFile "test/fixtures/keypair_truncated.json")
            case r of
              Left (_ :: IOException) -> pure ()
              Right _ -> assertFailure "accepted a 1-byte key file",
          testCase "rejects out-of-range byte values" $ do
            r <- try (readSigningKeyFromFile "test/fixtures/keypair_out_of_range.json")
            case r of
              Left (_ :: IOException) -> pure ()
              Right _ -> assertFailure "accepted a key file with an entry of 256",
          -- A JSON parser's syntax error quotes the unparsed remainder of its
          -- input; for a key file that would put secret bytes into an exception
          -- applications routinely log.
          testCase "does not echo the file's contents in the error for a malformed file" $ do
            r <- try (readSigningKeyFromFile "test/fixtures/keypair_syntax_error.json")
            case r of
              Left (e :: IOException) -> do
                assertBool ("unexpected message: " <> show e) ("not a JSON array of 64 integers" `isInfixOf` show e)
                assertBool
                  ("key bytes leaked into: " <> show e)
                  (not ("1,1,1" `isInfixOf` show e) && not ("138" `isInfixOf` show e))
              Right _ -> assertFailure "accepted a malformed key file"
        ],
      testGroup
        "raw constructors"
        [ testCase "unsafeSolanaPrivateKeyRaw calls error on a wrong-length input" $ do
            r <- try (evaluate (unsafeSolanaPrivateKeyRaw [1]))
            case r of
              Left (_ :: ErrorCall) -> pure ()
              Right _ -> assertFailure "built a 1-byte private key",
          testCase "unsafeSolanaPublicKeyRaw calls error on a wrong-length input" $ do
            r <- try (evaluate (unsafeSolanaPublicKeyRaw (replicate 33 0)))
            case r of
              Left (_ :: ErrorCall) -> pure ()
              Right _ -> assertFailure "built a 33-byte public key",
          testCase "unsafeSolanaPrivateKey calls error on a mismatched public-key half" $ do
            let raw = getSolanaPrivateKeyRaw (snd fixtureKeypair)
                corrupted = toBase58String (BS.take 32 raw <> BS.replicate 32 9)
            r <- try (evaluate (unsafeSolanaPrivateKey corrupted))
            case r of
              Left (_ :: ErrorCall) -> pure ()
              Right _ -> assertFailure "built a private key whose public half is not derived from its seed"
        ]
    ]
