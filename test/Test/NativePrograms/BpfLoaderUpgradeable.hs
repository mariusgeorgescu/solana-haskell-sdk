{-# LANGUAGE OverloadedStrings #-}

module Test.NativePrograms.BpfLoaderUpgradeable (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, getSolanaPublicKeyRaw, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.NativePrograms.BpfLoaderUpgradeable qualified as Loader
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.Sysvar qualified as Sysvar
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

bufferPk, programPk, spillPk, newAuthPk :: SolanaPublicKey
bufferPk = unsafeSolanaPublicKeyRaw (replicate 32 24)
programPk = unsafeSolanaPublicKeyRaw (replicate 32 23)
spillPk = unsafeSolanaPublicKeyRaw (replicate 32 22)
newAuthPk = unsafeSolanaPublicKeyRaw (replicate 32 20)

programDataPk, recipientPk, authorityPk' :: SolanaPublicKey
programDataPk = unsafeSolanaPublicKeyRaw (replicate 32 25)
recipientPk = unsafeSolanaPublicKeyRaw (replicate 32 26)
authorityPk' = unsafeSolanaPublicKeyRaw (replicate 32 27)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

enc :: Loader.UpgradeableLoaderInstruction -> BS.ByteString
enc = BL.toStrict . encode

genLoaderInstruction :: Gen Loader.UpgradeableLoaderInstruction
genLoaderInstruction =
  oneof
    [ pure Loader.InitializeBuffer,
      Loader.Write <$> arbitrary <*> (BS.pack <$> listOf arbitrary),
      Loader.DeployWithMaxDataLen <$> arbitrary,
      pure Loader.Upgrade,
      pure Loader.SetAuthority,
      pure Loader.Close,
      Loader.ExtendProgram <$> arbitrary,
      pure Loader.SetAuthorityChecked
    ]

tests :: TestTree
tests =
  withResource (loadFixtures "test/fixtures/loader_instruction_data.json") (const (pure ())) $ \getFixtures ->
    withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getPdaFixtures ->
      testGroup
        "BpfLoaderUpgradeable instruction data (golden + properties)"
        [ goldenCase getFixtures "InitializeBuffer" Loader.InitializeBuffer,
          goldenCase getFixtures "Write" (Loader.Write 128 (BS.replicate 64 0xAB)),
          goldenCase getFixtures "DeployWithMaxDataLen" (Loader.DeployWithMaxDataLen 1048576),
          goldenCase getFixtures "Upgrade" Loader.Upgrade,
          goldenCase getFixtures "SetAuthority" Loader.SetAuthority,
          goldenCase getFixtures "Close" Loader.Close,
          goldenCase getFixtures "ExtendProgram" (Loader.ExtendProgram 4096),
          goldenCase getFixtures "SetAuthorityChecked" Loader.SetAuthorityChecked,
          testProperty "Binary round-trip" $
            forAll genLoaderInstruction $ \li -> decode (encode li) === li,
          testCase "decode fails on unknown discriminant" $
            case decodeOrFail (BL.pack [8, 0, 0, 0]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Loader.UpgradeableLoaderInstruction) of
              Left _ -> pure ()
              Right _ -> assertFailure "expected decode failure for discriminant 8",
          testCase "programDataAddress matches Rust" $ do
            fs <- getPdaFixtures
            case Loader.programDataAddress programPk of
              Nothing -> assertFailure "no programdata PDA found"
              Just addr -> getSolanaPublicKeyRaw addr @?= requireFixture "programdata-address" fs,
          testCase "initializeBuffer metas" $
            iAccounts (Loader.initializeBuffer bufferPk payerPk)
              @?= [ AccountMeta bufferPk False True,
                    AccountMeta payerPk False False
                  ],
          testCase "write metas" $
            iAccounts (Loader.write bufferPk payerPk 128 (BS.replicate 64 0xAB))
              @?= [ AccountMeta bufferPk False True,
                    AccountMeta payerPk True False
                  ],
          testCase "deployWithMaxDataLen metas" $
            iAccounts (Loader.deployWithMaxDataLen payerPk programDataPk programPk bufferPk authorityPk' 1048576)
              @?= [ AccountMeta payerPk True True,
                    AccountMeta programDataPk False True,
                    AccountMeta programPk False True,
                    AccountMeta bufferPk False True,
                    AccountMeta Sysvar.rent False False,
                    AccountMeta Sysvar.clock False False,
                    AccountMeta SP.systemProgramId False False,
                    AccountMeta authorityPk' True False
                  ],
          testCase "upgrade metas" $
            iAccounts (Loader.upgrade programDataPk programPk bufferPk spillPk payerPk)
              @?= [ AccountMeta programDataPk False True,
                    AccountMeta programPk False True,
                    AccountMeta bufferPk False True,
                    AccountMeta spillPk False True,
                    AccountMeta Sysvar.rent False False,
                    AccountMeta Sysvar.clock False False,
                    AccountMeta payerPk True False
                  ],
          testCase "setAuthority metas" $
            iAccounts (Loader.setAuthority bufferPk payerPk newAuthPk)
              @?= [ AccountMeta bufferPk False True,
                    AccountMeta payerPk True False,
                    AccountMeta newAuthPk False False
                  ],
          testCase "setAuthorityChecked metas" $
            iAccounts (Loader.setAuthorityChecked bufferPk payerPk newAuthPk)
              @?= [ AccountMeta bufferPk False True,
                    AccountMeta payerPk True False,
                    AccountMeta newAuthPk True False
                  ],
          testCase "closeAccount metas (no associated program)" $
            iAccounts (Loader.closeAccount bufferPk recipientPk payerPk Nothing)
              @?= [ AccountMeta bufferPk False True,
                    AccountMeta recipientPk False True,
                    AccountMeta payerPk True False
                  ],
          testCase "closeAccount metas (programdata close)" $
            iAccounts (Loader.closeAccount programDataPk recipientPk payerPk (Just programPk))
              @?= [ AccountMeta programDataPk False True,
                    AccountMeta recipientPk False True,
                    AccountMeta payerPk True False,
                    AccountMeta programPk False True
                  ],
          testCase "extendProgram metas (no payer)" $
            iAccounts (Loader.extendProgram programDataPk programPk Nothing 4096)
              @?= [ AccountMeta programDataPk False True,
                    AccountMeta programPk False True
                  ],
          testCase "extendProgram metas (with payer)" $
            iAccounts (Loader.extendProgram programDataPk programPk (Just payerPk) 4096)
              @?= [ AccountMeta programDataPk False True,
                    AccountMeta programPk False True,
                    AccountMeta SP.systemProgramId False False,
                    AccountMeta payerPk True True
                  ]
        ]
  where
    goldenCase getFixtures name li =
      testCase name $ do
        fs <- getFixtures
        enc li @?= requireFixture name fs
