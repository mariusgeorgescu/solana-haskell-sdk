{-# LANGUAGE OverloadedStrings #-}

module Test.Metaplex.TokenMetadata (tests) where

import Data.Aeson (FromJSON (..), Value, eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson.Types (parseMaybe)
import Data.Binary (decode, decodeOrFail, encode)
import Data.Binary.Get (ByteOffset)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.List (find, isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeypairFromSeed, getSolanaPublicKeyRaw, unsafeSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction (AccountMeta (..), iAccounts)
import Network.Solana.Metaplex.TokenMetadata qualified as TM
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.SplPrograms.Token qualified as Token
import Network.Solana.Sysvar qualified as Sysvar
import Test.Fixtures
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- Fixed constants (ratified in Task 1 against the mpl-token-metadata crate).

mintPk :: SolanaPublicKey
mintPk = unsafeSolanaPublicKeyRaw (replicate 32 12)

payerPk :: SolanaPublicKey
payerPk =
  case createSolanaKeypairFromSeed (BS.replicate 32 1) of
    Just (pk, _) -> pk
    Nothing -> error "failed to derive payer"

secondCreatorPk :: SolanaPublicKey
secondCreatorPk = unsafeSolanaPublicKeyRaw (replicate 32 33)

collectionKeyPk :: SolanaPublicKey
collectionKeyPk = unsafeSolanaPublicKeyRaw (replicate 32 34)

dataV2Full :: TM.DataV2
dataV2Full =
  TM.DataV2
    { TM.dName = "Solana Haskell NFT",
      TM.dSymbol = "SHSDK",
      TM.dUri = "https://example.com/nft.json",
      TM.dSellerFeeBasisPoints = 550,
      TM.dCreators = Just [TM.Creator payerPk True 60, TM.Creator secondCreatorPk False 40],
      TM.dCollection = Just (TM.Collection False collectionKeyPk),
      TM.dUses = Just (TM.Uses TM.Multiple 10 10)
    }

dataV2Minimal :: TM.DataV2
dataV2Minimal =
  TM.DataV2
    { TM.dName = "Solana Haskell NFT",
      TM.dSymbol = "SHSDK",
      TM.dUri = "https://example.com/nft.json",
      TM.dSellerFeeBasisPoints = 550,
      TM.dCreators = Nothing,
      TM.dCollection = Nothing,
      TM.dUses = Nothing
    }

-- metadata_instruction_data.json mixes two shapes in one array: six
-- {name, hex} entries (the golden vectors) and one {name, accounts} entry
-- (create-v3-full-accounts). Test.Fixtures.loadFixtures decodes the whole
-- file as [Fixture], which requires every element to carry a "hex" field, so
-- it can't be used directly here. Load the file as [Value] instead and pick
-- out each shape locally, without extending the shared loader.

-- | Like 'loadFixtures', but tolerant of the extra {name, accounts} entry:
-- elements that don't parse as a {name, hex} 'Fixture' are simply skipped.
loadDataFixtures :: FilePath -> IO [Fixture]
loadDataFixtures path = do
  vals <- either fail pure =<< eitherDecodeFileStrict path
  pure (mapMaybe (parseMaybe parseJSON) (vals :: [Value]))

-- Local decode of the `create-v3-full-accounts` entry (ground truth for the
-- createMetadataAccountV3 meta test).

data FixtureAccountMeta = FixtureAccountMeta T.Text Bool Bool

instance FromJSON FixtureAccountMeta where
  parseJSON = withObject "FixtureAccountMeta" $ \v ->
    FixtureAccountMeta <$> v .: "pubkey" <*> v .: "signer" <*> v .: "writable"

data AccountsFixture = AccountsFixture
  { afName :: String,
    afAccounts :: [FixtureAccountMeta]
  }

instance FromJSON AccountsFixture where
  parseJSON = withObject "AccountsFixture" $ \v ->
    AccountsFixture <$> v .: "name" <*> v .: "accounts"

loadCreateV3FullAccounts :: FilePath -> IO [AccountMeta]
loadCreateV3FullAccounts path = do
  vals <- either fail pure =<< eitherDecodeFileStrict path
  case find ((== "create-v3-full-accounts") . afName) (mapMaybe (parseMaybe parseJSON) (vals :: [Value])) of
    Just af -> pure (map toAccountMeta (afAccounts af))
    Nothing -> fail "create-v3-full-accounts entry not found"
  where
    toAccountMeta (FixtureAccountMeta hexPubkey signer writable) =
      AccountMeta
        { accountPubKey = unsafeSolanaPublicKeyRaw (BS.unpack (decodeHex hexPubkey)),
          isSigner = signer,
          isWritable = writable
        }
    decodeHex hexPubkey = either (error . ("bad hex in create-v3-full-accounts: " <>)) id (B16.decode (TE.encodeUtf8 hexPubkey))

-- Small QuickCheck generators covering every support type and instruction
-- variant, for the round-trip property below.

genPk :: Gen SolanaPublicKey
genPk = unsafeSolanaPublicKeyRaw <$> vectorOf 32 arbitrary

genAsciiString :: Gen String
genAsciiString = listOf (elements ['a' .. 'z'])

genUseMethod :: Gen TM.UseMethod
genUseMethod = elements [TM.Burn, TM.Multiple, TM.Single]

genCreator :: Gen TM.Creator
genCreator = TM.Creator <$> genPk <*> arbitrary <*> arbitrary

genCollection :: Gen TM.Collection
genCollection = TM.Collection <$> arbitrary <*> genPk

genUses :: Gen TM.Uses
genUses = TM.Uses <$> genUseMethod <*> arbitrary <*> arbitrary

genCollectionDetails :: Gen TM.CollectionDetails
genCollectionDetails = TM.CollectionDetailsV1 <$> arbitrary

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

genDataV2 :: Gen TM.DataV2
genDataV2 =
  TM.DataV2
    <$> genAsciiString
    <*> genAsciiString
    <*> genAsciiString
    <*> arbitrary
    <*> genMaybe (resize 3 (listOf genCreator))
    <*> genMaybe genCollection
    <*> genMaybe genUses

genTokenMetadataInstruction :: Gen TM.TokenMetadataInstruction
genTokenMetadataInstruction =
  oneof
    [ TM.CreateMetadataAccountV3 <$> genDataV2 <*> arbitrary <*> genMaybe genCollectionDetails,
      TM.UpdateMetadataAccountV2
        <$> genMaybe genDataV2
        <*> genMaybe genPk
        <*> genMaybe arbitrary
        <*> genMaybe arbitrary,
      TM.CreateMasterEditionV3 <$> genMaybe arbitrary
    ]

enc :: TM.TokenMetadataInstruction -> BS.ByteString
enc = BL.toStrict . encode

tests :: TestTree
tests =
  withResource (loadDataFixtures "test/fixtures/metadata_instruction_data.json") (const (pure ())) $ \getFixtures ->
    withResource (loadFixtures "test/fixtures/pda.json") (const (pure ())) $ \getPdaFixtures ->
      withResource (loadCreateV3FullAccounts "test/fixtures/metadata_instruction_data.json") (const (pure ())) $ \getCreateV3Accounts ->
        withResource (loadFixtures "test/fixtures/state_fixtures.json") (const (pure ())) $ \getStateFixtures ->
          testGroup
            "Metaplex Token Metadata"
            [ testGroup
                "Metadata account state (golden + rejection)"
                [ testCase "decodeMetadata: metadata-account golden (full tail)" $ do
                    fs <- getStateFixtures
                    let bs = requireFixture "metadata-account" fs
                    case TM.decodeMetadata bs of
                      Left err -> assertFailure $ "decode failed: " <> err
                      Right md -> do
                        TM.mdKey md @?= 4
                        TM.mdUpdateAuthority md @?= payerPk
                        TM.mdMint md @?= mintPk
                        TM.mdName md @?= "Solana Haskell NFT"
                        TM.mdSymbol md @?= "SHSDK"
                        TM.mdUri md @?= "https://example.com/nft.json"
                        TM.mdSellerFeeBasisPoints md @?= 550
                        TM.mdCreators md @?= Just [TM.Creator payerPk True 60, TM.Creator secondCreatorPk False 40]
                        TM.mdPrimarySaleHappened md @?= False
                        TM.mdIsMutable md @?= True
                        TM.mdEditionNonce md @?= Just 253
                        TM.mdTokenStandard md @?= Just 0
                        TM.mdCollection md @?= Just (TM.Collection False collectionKeyPk)
                        TM.mdUses md @?= Just (TM.Uses TM.Multiple 10 10),
                  testCase "decodeMetadata: metadata-account-legacy golden (empty tail)" $ do
                    fs <- getStateFixtures
                    let bs = requireFixture "metadata-account-legacy" fs
                    case TM.decodeMetadata bs of
                      Left err -> assertFailure $ "decode failed: " <> err
                      Right md -> do
                        TM.mdKey md @?= 4
                        TM.mdUpdateAuthority md @?= payerPk
                        TM.mdMint md @?= mintPk
                        TM.mdName md @?= "Solana Haskell NFT"
                        TM.mdSymbol md @?= "SHSDK"
                        TM.mdUri md @?= "https://example.com/nft.json"
                        TM.mdSellerFeeBasisPoints md @?= 550
                        TM.mdCreators md @?= Just [TM.Creator payerPk True 60, TM.Creator secondCreatorPk False 40]
                        TM.mdPrimarySaleHappened md @?= False
                        TM.mdIsMutable md @?= True
                        TM.mdEditionNonce md @?= Nothing
                        TM.mdTokenStandard md @?= Nothing
                        TM.mdCollection md @?= Nothing
                        TM.mdUses md @?= Nothing,
                  testCase "decodeMetadata: rejects empty bytes" $
                    case TM.decodeMetadata BS.empty of
                      Left _ -> pure ()
                      Right _ -> assertFailure "expected decode failure for empty bytes",
                  testCase "decodeMetadata: rejects truncated mid-pubkey (40 bytes)" $
                    case TM.decodeMetadata (BS.replicate 40 0) of
                      Left _ -> pure ()
                      Right _ -> assertFailure "expected decode failure for 40 bytes",
                  testCase "decodeMetadata: rejects bad discriminator (key byte != 4)" $ do
                    fs <- getStateFixtures
                    let bs = requireFixture "metadata-account" fs
                    let corrupted = BS.cons 5 (BS.drop 1 bs)
                    case TM.decodeMetadata corrupted of
                      Left err -> assertBool "error message includes 'unexpected account key'" $
                        "unexpected account key" `isInfixOf` err
                      Right _ -> assertFailure "expected decode failure for bad discriminator"
                ],
              testGroup
                "PDA derivation"
                [ testCase "deriveMetadataAddress matches fixture" $ do
                    fs <- getPdaFixtures
                    case TM.deriveMetadataAddress mintPk of
                      Nothing -> assertFailure "no metadata PDA found"
                      Just addr -> getSolanaPublicKeyRaw addr @?= requireFixture "metadata-address" fs,
                  testCase "deriveMasterEditionAddress matches fixture" $ do
                    fs <- getPdaFixtures
                    case TM.deriveMasterEditionAddress mintPk of
                      Nothing -> assertFailure "no master edition PDA found"
                      Just addr -> getSolanaPublicKeyRaw addr @?= requireFixture "master-edition-address" fs
                ],
              testGroup
                "instruction data (golden)"
                [ goldenCase getFixtures "CreateMetadataAccountV3-full" (TM.CreateMetadataAccountV3 dataV2Full True (Just (TM.CollectionDetailsV1 0))),
                  goldenCase getFixtures "CreateMetadataAccountV3-minimal" (TM.CreateMetadataAccountV3 dataV2Minimal True Nothing),
                  goldenCase getFixtures "UpdateMetadataAccountV2-some" (TM.UpdateMetadataAccountV2 (Just dataV2Full) (Just payerPk) (Just True) (Just True)),
                  goldenCase getFixtures "UpdateMetadataAccountV2-none" (TM.UpdateMetadataAccountV2 Nothing Nothing Nothing Nothing),
                  goldenCase getFixtures "CreateMasterEditionV3-some" (TM.CreateMasterEditionV3 (Just 100)),
                  goldenCase getFixtures "CreateMasterEditionV3-none" (TM.CreateMasterEditionV3 Nothing)
                ],
              testGroup
                "properties"
                [ testProperty "Binary round-trip" $
                    forAll genTokenMetadataInstruction $ \i -> decode (encode i) === i,
                  testCase "decode fails on unknown discriminant" $
                    case decodeOrFail (BL.pack [200]) :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, TM.TokenMetadataInstruction) of
                      Left _ -> pure ()
                      Right _ -> assertFailure "expected decode failure for discriminant 200"
                ],
              testGroup
                "builder metas"
                [ testCase "createMetadataAccountV3 matches create-v3-full-accounts fixture" $ do
                    expected <- getCreateV3Accounts
                    iAccounts (TM.createMetadataAccountV3 mintPk payerPk payerPk payerPk True dataV2Full True (Just (TM.CollectionDetailsV1 0)))
                      @?= expected,
                  testCase "updateMetadataAccountV2 metas" $
                    case TM.deriveMetadataAddress mintPk of
                      Nothing -> assertFailure "no metadata PDA found"
                      Just metadataAddr ->
                        iAccounts (TM.updateMetadataAccountV2 mintPk payerPk Nothing Nothing Nothing Nothing)
                          @?= [ AccountMeta {accountPubKey = metadataAddr, isSigner = False, isWritable = True},
                                AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = False}
                              ],
                  testCase "createMasterEditionV3 metas" $
                    case (TM.deriveMasterEditionAddress mintPk, TM.deriveMetadataAddress mintPk) of
                      (Just editionAddr, Just metadataAddr) ->
                        iAccounts (TM.createMasterEditionV3 mintPk payerPk payerPk payerPk (Just 100))
                          @?= [ AccountMeta {accountPubKey = editionAddr, isSigner = False, isWritable = True},
                                AccountMeta {accountPubKey = mintPk, isSigner = False, isWritable = True},
                                AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = False},
                                AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = False},
                                AccountMeta {accountPubKey = payerPk, isSigner = True, isWritable = True},
                                AccountMeta {accountPubKey = metadataAddr, isSigner = False, isWritable = False},
                                AccountMeta {accountPubKey = Token.tokenProgramId, isSigner = False, isWritable = False},
                                AccountMeta {accountPubKey = SystemProgram.systemProgramId, isSigner = False, isWritable = False},
                                AccountMeta {accountPubKey = Sysvar.rent, isSigner = False, isWritable = False}
                              ]
                      _ -> assertFailure "PDA derivation failed"
                ]
            ]
  where
    goldenCase getFixtures name i =
      testCase name $ do
        fs <- getFixtures
        enc i @?= requireFixture name fs
