{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.Token (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.SplPrograms.Token qualified as Tok
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

walletPk, delegatePk :: SolanaPublicKey
walletPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
delegatePk = unsafeSolanaPublicKeyRaw (replicate 32 13)

enc :: Tok.TokenInstruction -> BS.ByteString
enc = BL.toStrict . encode

genTokenInstruction :: Gen Tok.TokenInstruction
genTokenInstruction =
  oneof
    [ Tok.InitializeMint <$> arbitrary <*> genPk <*> genMaybePk,
      pure Tok.InitializeAccount,
      Tok.InitializeMultisig <$> arbitrary,
      Tok.Transfer <$> arbitrary,
      Tok.Approve <$> arbitrary,
      pure Tok.Revoke,
      Tok.SetAuthority <$> genAuthType <*> genMaybePk,
      Tok.MintTo <$> arbitrary,
      Tok.Burn <$> arbitrary,
      pure Tok.CloseAccount,
      pure Tok.FreezeAccount,
      pure Tok.ThawAccount,
      Tok.TransferChecked <$> arbitrary <*> arbitrary,
      Tok.ApproveChecked <$> arbitrary <*> arbitrary,
      Tok.MintToChecked <$> arbitrary <*> arbitrary,
      Tok.BurnChecked <$> arbitrary <*> arbitrary,
      Tok.InitializeAccount2 <$> genPk,
      pure Tok.SyncNative,
      Tok.InitializeAccount3 <$> genPk,
      Tok.InitializeMultisig2 <$> arbitrary,
      Tok.InitializeMint2 <$> arbitrary <*> genPk <*> genMaybePk
    ]
  where
    genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary
    genMaybePk = oneof [pure Nothing, Just <$> genPk]
    genAuthType = elements [Tok.MintTokens, Tok.FreezeAuthority, Tok.AccountOwner, Tok.CloseAuthority]

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/token_instruction_data.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "SPL Token instruction data (golden + properties)"
      [ goldenCase getFixtures "InitializeMint-some" (Tok.InitializeMint 6 walletPk (Just delegatePk)),
        goldenCase getFixtures "InitializeMint-none" (Tok.InitializeMint 6 walletPk Nothing),
        goldenCase getFixtures "InitializeAccount" Tok.InitializeAccount,
        goldenCase getFixtures "InitializeMultisig" (Tok.InitializeMultisig 2),
        goldenCase getFixtures "Transfer" (Tok.Transfer 1000000),
        goldenCase getFixtures "Approve" (Tok.Approve 1000000),
        goldenCase getFixtures "Revoke" Tok.Revoke,
        goldenCase getFixtures "SetAuthority-some" (Tok.SetAuthority Tok.AccountOwner (Just delegatePk)),
        goldenCase getFixtures "SetAuthority-none" (Tok.SetAuthority Tok.CloseAuthority Nothing),
        goldenCase getFixtures "MintTo" (Tok.MintTo 1000000),
        goldenCase getFixtures "Burn" (Tok.Burn 1000000),
        goldenCase getFixtures "CloseAccount" Tok.CloseAccount,
        goldenCase getFixtures "FreezeAccount" Tok.FreezeAccount,
        goldenCase getFixtures "ThawAccount" Tok.ThawAccount,
        goldenCase getFixtures "TransferChecked" (Tok.TransferChecked 1000000 6),
        goldenCase getFixtures "ApproveChecked" (Tok.ApproveChecked 1000000 6),
        goldenCase getFixtures "MintToChecked" (Tok.MintToChecked 1000000 6),
        goldenCase getFixtures "BurnChecked" (Tok.BurnChecked 1000000 6),
        goldenCase getFixtures "InitializeAccount2" (Tok.InitializeAccount2 walletPk),
        goldenCase getFixtures "SyncNative" Tok.SyncNative,
        goldenCase getFixtures "InitializeAccount3" (Tok.InitializeAccount3 walletPk),
        goldenCase getFixtures "InitializeMultisig2" (Tok.InitializeMultisig2 2),
        goldenCase getFixtures "InitializeMint2-some" (Tok.InitializeMint2 6 walletPk (Just delegatePk)),
        testProperty "Binary round-trip" $
          forAll genTokenInstruction $ \ti -> decode (encode ti) === ti,
        testCase "decode fails on unknown discriminant" $
          case decodeOrFail (BL.pack [21]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Tok.TokenInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 21"
      ]
  where
    goldenCase getFixtures name ti =
      testCase name $ do
        fs <- getFixtures
        enc ti @?= requireFixture name fs
