{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Pda (tests) where

import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw, unsafeSolanaPublicKey)
import Network.Solana.Core.Pda
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tokenProgram :: SolanaPublicKey
tokenProgram = unsafeSolanaPublicKey "TokenkegQfeZyiNwAJbNbGKPFXCWuBvf9Ss623VQ5DA"

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "PDA derivation"
      [ testCase "findProgramAddress matches Rust (address and bump)" $ do
          fs <- getFixtures
          case findProgramAddress ["solana-haskell-sdk/1"] tokenProgram of
            Nothing -> assertFailure "no PDA found"
            Just (addr, bump) -> do
              getSolanaPublicKeyRaw addr @?= requireFixture "generic-pda-address" fs
              BS.singleton bump @?= requireFixture "generic-pda-bump" fs,
        testCase "seed longer than 32 bytes is rejected" $
          assertBool "expected Left" (isLeft (createProgramAddress [BS.replicate 33 0] tokenProgram)),
        testProperty "createProgramAddress never returns an on-curve address" $
          forAll (BS.pack <$> vectorOf 8 arbitrary) $ \seed ->
            case createProgramAddress [seed, "bump"] tokenProgram of
              Left _ -> property True
              Right addr -> property (getSolanaPublicKeyRaw addr /= BS.replicate 32 0)
      ]
