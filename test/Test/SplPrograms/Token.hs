{-# LANGUAGE OverloadedStrings #-}

module Test.SplPrograms.Token (tests) where

import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf)
import Network.Solana.Core.Crypto (SolanaPublicKey, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.SplPrograms.Token qualified as Tok
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

payerPk :: SolanaPublicKey
payerPk = unsafeSolanaPublicKeyRaw (replicate 32 11)

ownerKey :: SolanaPublicKey
ownerKey = unsafeSolanaPublicKeyRaw (map fromIntegral [0x8a :: Int, 0x88, 0xe3, 0xdd, 0x74, 0x09, 0xf1, 0x95, 0xfd, 0x52, 0xdb, 0x2d, 0x3c, 0xba, 0x5d, 0x72, 0xca, 0x67, 0x09, 0xbf, 0x1d, 0x94, 0x12, 0x1b, 0xf3, 0x74, 0x88, 0x01, 0xb4, 0x0f, 0x6f, 0x5c])

walletPk, delegatePk, mintPk, sig1Pk, sig2Pk, accountPk :: SolanaPublicKey
walletPk = unsafeSolanaPublicKeyRaw (replicate 32 11)
delegatePk = unsafeSolanaPublicKeyRaw (replicate 32 13)
mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)
sig1Pk = unsafeSolanaPublicKeyRaw (replicate 32 14)
sig2Pk = unsafeSolanaPublicKeyRaw (replicate 32 15)
accountPk = unsafeSolanaPublicKeyRaw (replicate 32 16)

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
  testGroup
    "SPL Token"
    [ withResource (loadFixtures "test/fixtures/token_instruction_data.json") (const (pure ())) $ \getFixtures ->
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
        goldenCase getFixtures "SetAuthority-mint-tokens" (Tok.SetAuthority Tok.MintTokens (Just delegatePk)),
        goldenCase getFixtures "SetAuthority-freeze" (Tok.SetAuthority Tok.FreezeAuthority Nothing),
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
        testCase "transfer metas (single owner)" $
          iAccounts (Tok.transfer accountPk delegatePk walletPk [] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ],
        testCase "transfer metas (multisig)" $
          iAccounts (Tok.transfer accountPk delegatePk walletPk [sig1Pk, sig2Pk] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk False False,
                  AccountMeta sig1Pk True False,
                  AccountMeta sig2Pk True False
                ],
        testCase "transferChecked metas" $
          iAccounts (Tok.transferChecked accountPk mintPk delegatePk walletPk [] 1000000 6)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ],
        testCase "initializeMint metas" $
          iAccounts (Tok.initializeMint mintPk 6 walletPk (Just delegatePk))
            @?= [ AccountMeta mintPk False True,
                  AccountMeta Tok.rentSysvar False False
                ],
        testCase "mintTo metas" $
          iAccounts (Tok.mintTo mintPk accountPk walletPk [] 1000000)
            @?= [ AccountMeta mintPk False True,
                  AccountMeta accountPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "burn metas" $
          iAccounts (Tok.burn accountPk mintPk walletPk [] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "closeAccount metas" $
          iAccounts (Tok.closeAccount accountPk delegatePk walletPk [])
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False True,
                  AccountMeta walletPk True False
                ],
        testCase "approve metas" $
          iAccounts (Tok.approve accountPk delegatePk walletPk [] 1000000)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta delegatePk False False,
                  AccountMeta walletPk True False
                ],
        testCase "revoke metas" $
          iAccounts (Tok.revoke accountPk walletPk [])
            @?= [ AccountMeta accountPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "setAuthority metas" $
          iAccounts (Tok.setAuthority mintPk Tok.AccountOwner (Just delegatePk) walletPk [])
            @?= [ AccountMeta mintPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "initializeAccount metas" $
          iAccounts (Tok.initializeAccount accountPk mintPk walletPk)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta walletPk False False,
                  AccountMeta Tok.rentSysvar False False
                ],
        testCase "freezeAccount metas" $
          iAccounts (Tok.freezeAccount accountPk mintPk walletPk [])
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta walletPk True False
                ],
        testCase "thawAccount metas" $
          iAccounts (Tok.thawAccount accountPk mintPk walletPk [])
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta walletPk True False
                ],
        testCase "syncNative metas" $
          iAccounts (Tok.syncNative accountPk)
            @?= [AccountMeta accountPk False True],
        testCase "initializeMultisig metas" $
          iAccounts (Tok.initializeMultisig accountPk [sig1Pk, sig2Pk] 2)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta Tok.rentSysvar False False,
                  AccountMeta sig1Pk False False,
                  AccountMeta sig2Pk False False
                ],
        testCase "initializeMultisig2 metas" $
          iAccounts (Tok.initializeMultisig2 accountPk [sig1Pk, sig2Pk] 2)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta sig1Pk False False,
                  AccountMeta sig2Pk False False
                ],
        testCase "approveChecked metas" $
          iAccounts (Tok.approveChecked accountPk mintPk delegatePk walletPk [] 1000000 6)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta delegatePk False False,
                  AccountMeta walletPk True False
                ],
        testCase "mintToChecked metas" $
          iAccounts (Tok.mintToChecked mintPk accountPk walletPk [] 1000000 6)
            @?= [ AccountMeta mintPk False True,
                  AccountMeta accountPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "burnChecked metas" $
          iAccounts (Tok.burnChecked accountPk mintPk walletPk [] 1000000 6)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False True,
                  AccountMeta walletPk True False
                ],
        testCase "initializeAccount2 (builder) metas" $
          iAccounts (Tok.initializeAccount2 accountPk mintPk walletPk)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False,
                  AccountMeta Tok.rentSysvar False False
                ],
        testCase "initializeAccount3 (builder) metas" $
          iAccounts (Tok.initializeAccount3 accountPk mintPk walletPk)
            @?= [ AccountMeta accountPk False True,
                  AccountMeta mintPk False False
                ],
        testCase "initializeMint2 (builder) metas" $
          iAccounts (Tok.initializeMint2 mintPk 6 walletPk (Just delegatePk))
            @?= [AccountMeta mintPk False True],
        testProperty "Binary round-trip" $
          forAll genTokenInstruction $ \ti -> decode (encode ti) === ti,
        testCase "decode fails on unknown discriminant" $
          case decodeOrFail (BL.pack [21]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, Tok.TokenInstruction) of
            Left _ -> pure ()
            Right _ -> assertFailure "expected decode failure for discriminant 21"
          ],
      withResource (loadFixtures "test/fixtures/state_fixtures.json") (const (pure ())) $ \getFixtures ->
        testGroup
          "SPL Token state decoders (golden + rejection)"
          [ testCase "decodeTokenAccount: token-account golden" $ do
              fs <- getFixtures
              let bs = requireFixture "token-account" fs
              case Tok.decodeTokenAccount bs of
                Left err -> assertFailure $ "decode failed: " <> err
                Right ta -> do
                  Tok.taMint ta @?= unsafeSolanaPublicKeyRaw (replicate 32 12)
                  Tok.taOwner ta @?= ownerKey
                  Tok.taAmount ta @?= 5000000
                  Tok.taDelegate ta @?= Just (unsafeSolanaPublicKeyRaw (replicate 32 13))
                  Tok.taState ta @?= Tok.TokenAccountFrozen
                  Tok.taIsNative ta @?= Just 2039280
                  Tok.taDelegatedAmount ta @?= 1000
                  Tok.taCloseAuthority ta @?= Just (unsafeSolanaPublicKeyRaw (replicate 32 20)),
            testCase "decodeTokenAccount: token-account-minimal golden" $ do
              fs <- getFixtures
              let bs = requireFixture "token-account-minimal" fs
              case Tok.decodeTokenAccount bs of
                Left err -> assertFailure $ "decode failed: " <> err
                Right ta -> do
                  Tok.taMint ta @?= unsafeSolanaPublicKeyRaw (replicate 32 12)
                  Tok.taOwner ta @?= ownerKey
                  Tok.taAmount ta @?= 5000000
                  Tok.taDelegate ta @?= Nothing
                  Tok.taState ta @?= Tok.TokenAccountInitialized
                  Tok.taIsNative ta @?= Nothing
                  Tok.taDelegatedAmount ta @?= 0
                  Tok.taCloseAuthority ta @?= Nothing,
            testCase "decodeMint: mint golden" $ do
              fs <- getFixtures
              let bs = requireFixture "mint" fs
              case Tok.decodeMint bs of
                Left err -> assertFailure $ "decode failed: " <> err
                Right m -> do
                  Tok.mMintAuthority m @?= Just ownerKey
                  Tok.mSupply m @?= 1000000000
                  Tok.mDecimals m @?= 6
                  Tok.mIsInitialized m @?= True
                  Tok.mFreezeAuthority m @?= Just (unsafeSolanaPublicKeyRaw (replicate 32 20)),
            testCase "decodeMint: mint-minimal golden" $ do
              fs <- getFixtures
              let bs = requireFixture "mint-minimal" fs
              case Tok.decodeMint bs of
                Left err -> assertFailure $ "decode failed: " <> err
                Right m -> do
                  Tok.mMintAuthority m @?= Nothing
                  Tok.mSupply m @?= 1000000000
                  Tok.mDecimals m @?= 6
                  Tok.mIsInitialized m @?= True
                  Tok.mFreezeAuthority m @?= Nothing,
            testCase "decodeTokenAccount: reject 164 bytes" $ do
              let bs = BS.replicate 164 0
              case Tok.decodeTokenAccount bs of
                Left err -> assertBool "error message contains length" ("164" `elem` words err)
                Right _ -> assertFailure "expected decode failure for 164 bytes",
            testCase "decodeTokenAccount: reject 166 bytes" $ do
              let bs = BS.replicate 166 0
              case Tok.decodeTokenAccount bs of
                Left err -> assertBool "error message contains length" ("166" `elem` words err)
                Right _ -> assertFailure "expected decode failure for 166 bytes",
            testCase "decodeTokenAccount: reject invalid COption tag" $ do
              let bs = BS.replicate 165 0xFF
              case Tok.decodeTokenAccount bs of
                Left err -> assertBool "error message contains COption tag" ("COption" `isInfixOf` err || "tag" `isInfixOf` err)
                Right _ -> assertFailure "expected decode failure for 0xFF bytes",
            testCase "decodeMint: reject 81 bytes" $ do
              let bs = BS.replicate 81 0
              case Tok.decodeMint bs of
                Left err -> assertBool "error message contains length" ("81" `elem` words err)
                Right _ -> assertFailure "expected decode failure for 81 bytes",
            testCase "decodeMint: reject 83 bytes" $ do
              let bs = BS.replicate 83 0
              case Tok.decodeMint bs of
                Left err -> assertBool "error message contains length" ("83" `elem` words err)
                Right _ -> assertFailure "expected decode failure for 83 bytes"
          ]
    ]
  where
    goldenCase getFixtures name ti =
      testCase name $ do
        fs <- getFixtures
        enc ti @?= requireFixture name fs
