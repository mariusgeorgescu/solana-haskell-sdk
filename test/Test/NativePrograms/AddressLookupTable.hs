{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.AddressLookupTable (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8)
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, getSolanaPublicKeyRaw, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.NativePrograms.AddressLookupTable qualified as ALT
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

tablePk, addrA, addrB, recipientPk, funderPk :: SolanaPublicKey
tablePk = unsafeSolanaPublicKeyRaw (replicate 32 31)
addrA = unsafeSolanaPublicKeyRaw (replicate 32 29)
addrB = unsafeSolanaPublicKeyRaw (replicate 32 30)
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 22)

-- | Distinct from 'payerPk' (used as the authority) so that a builder
-- accidentally swapping its authority/payer arguments would be caught by
-- the meta tests below.
funderPk = unsafeSolanaPublicKeyRaw (replicate 32 32)

-- | The bump seed for (payerPk, 12345), used to build the CreateLookupTable
-- golden without hard-coding it (it must equal the derivation, which is
-- separately checked against the Rust fixture).
lutBump :: Word8
lutBump =
  case ALT.deriveLookupTableAddress payerPk 12345 of
    Just (_, bump) -> bump
    Nothing -> error "no lookup table PDA found"

enc :: ALT.AddressLookupTableInstruction -> BS.ByteString
enc = BL.toStrict . encode

genAltInstruction :: Gen ALT.AddressLookupTableInstruction
genAltInstruction =
  oneof
    [ ALT.CreateLookupTable <$> arbitrary <*> arbitrary,
      pure ALT.FreezeLookupTable,
      ALT.ExtendLookupTable <$> listOf genPubkey,
      pure ALT.DeactivateLookupTable,
      pure ALT.CloseLookupTable
    ]
  where
    genPubkey = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/alt_instruction_data.json") (const (pure ())) $ \getFixtures ->
    withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getPdaFixtures ->
      testGroup
        "AddressLookupTable instruction data (golden + properties)"
        [ testCase "deriveLookupTableAddress matches Rust (address and bump)" $ do
            fs <- getPdaFixtures
            case ALT.deriveLookupTableAddress payerPk 12345 of
              Nothing -> assertFailure "no lookup table PDA found"
              Just (addr, bump) -> do
                getSolanaPublicKeyRaw addr @?= requireFixture "lookup-table-address" fs
                BS.singleton bump @?= requireFixture "lookup-table-bump" fs,
          goldenCase getFixtures "CreateLookupTable" (ALT.CreateLookupTable 12345 lutBump),
          goldenCase getFixtures "FreezeLookupTable" ALT.FreezeLookupTable,
          goldenCase getFixtures "ExtendLookupTable" (ALT.ExtendLookupTable [addrA, addrB]),
          goldenCase getFixtures "DeactivateLookupTable" ALT.DeactivateLookupTable,
          goldenCase getFixtures "CloseLookupTable" ALT.CloseLookupTable,
          testProperty "Binary round-trip" $
            forAll genAltInstruction $ \ai -> decode (encode ai) === ai,
          testCase "decode fails on unknown discriminant" $
            case decodeOrFail (BL.pack [5, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, ALT.AddressLookupTableInstruction) of
              Left _ -> pure ()
              Right _ -> assertFailure "expected decode failure for discriminant 5",
          testCase "createLookupTable metas (and returned table address)" $ do
            fs <- getPdaFixtures
            let (ix, table) = ALT.createLookupTable payerPk funderPk 12345
            getSolanaPublicKeyRaw table @?= requireFixture "lookup-table-address" fs
            iAccounts ix
              @?= [ AccountMeta table False True,
                    AccountMeta payerPk False False,
                    AccountMeta funderPk True True,
                    AccountMeta SP.systemProgramId False False
                  ],
          testCase "freezeLookupTable metas" $
            iAccounts (ALT.freezeLookupTable tablePk payerPk)
              @?= [ AccountMeta tablePk False True,
                    AccountMeta payerPk True False
                  ],
          testCase "extendLookupTable metas (no payer)" $
            iAccounts (ALT.extendLookupTable tablePk payerPk Nothing [addrA, addrB])
              @?= [ AccountMeta tablePk False True,
                    AccountMeta payerPk True False
                  ],
          testCase "extendLookupTable metas (with payer)" $
            iAccounts (ALT.extendLookupTable tablePk payerPk (Just funderPk) [addrA, addrB])
              @?= [ AccountMeta tablePk False True,
                    AccountMeta payerPk True False,
                    AccountMeta funderPk True True,
                    AccountMeta SP.systemProgramId False False
                  ],
          testCase "deactivateLookupTable metas" $
            iAccounts (ALT.deactivateLookupTable tablePk payerPk)
              @?= [ AccountMeta tablePk False True,
                    AccountMeta payerPk True False
                  ],
          testCase "closeLookupTable metas" $
            iAccounts (ALT.closeLookupTable tablePk payerPk recipientPk)
              @?= [ AccountMeta tablePk False True,
                    AccountMeta payerPk True False,
                    AccountMeta recipientPk False True
                  ]
        ]
  where
    goldenCase getFixtures name li =
      testCase name $ do
        fs <- getFixtures
        enc li @?= requireFixture name fs
