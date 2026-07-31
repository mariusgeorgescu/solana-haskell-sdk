{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.AssociatedTokenAccount (tests) where

import Data.ByteString qualified as BS
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata
import Network.Solana.SplPrograms.Token qualified as Tok
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit

mintPk :: SolanaPublicKey
mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getFixtures ->
    testGroup
      "Associated Token Account"
      [ testCase "getAssociatedTokenAddress matches Rust" $ do
          fs <- getFixtures
          case Ata.getAssociatedTokenAddress payerPk mintPk of
            Nothing -> assertFailure "no ATA derivable"
            Just ata -> getSolanaPublicKeyRaw ata @?= requireFixture "ata-address" fs,
        testCase "create-idempotent data byte" $ do
          case Ata.getAssociatedTokenAddress payerPk mintPk of
            Nothing -> assertFailure "no ATA derivable"
            Just _ ->
              instrData (iData (Ata.createAssociatedTokenAccountIdempotent payerPk payerPk mintPk))
                @?= BS.singleton 1,
        testCase "create data byte" $
          instrData (iData (Ata.createAssociatedTokenAccount payerPk payerPk mintPk))
            @?= BS.singleton 0,
        testCase "create metas" $ do
          fs <- getFixtures
          let expectedAta = unsafeSolanaPublicKeyRaw (BS.unpack (requireFixture "ata-address" fs))
          iAccounts (Ata.createAssociatedTokenAccount payerPk payerPk mintPk)
            @?= [ AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = True},
                  AccountMeta {accountPubKey = expectedAta, isSigner = False, isWritable = True},
                  AccountMeta {accountPubKey = payerPk, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = mintPk, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = Ata.systemProgramRef, isSigner = False, isWritable = False},
                  AccountMeta {accountPubKey = Tok.tokenProgramId, isSigner = False, isWritable = False}
                ]
      ]
