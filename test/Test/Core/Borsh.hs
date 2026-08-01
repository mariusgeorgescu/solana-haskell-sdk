{-# LANGUAGE OverloadedStrings #-}

module Test.Core.Borsh (tests) where

import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (isNothing)
import Network.Solana.Core.Borsh
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

tests :: TestTree
tests =
  testGroup
    "borsh"
    [ testGroup
        "string"
        [ testCase "putBorshString produces u32-prefixed UTF-8" $
            let bs = BL.toStrict (runPut (putBorshString "ab"))
                expected = BS.pack [0x02, 0x00, 0x00, 0x00, 0x61, 0x62]
             in bs @?= expected,
          testCase "putBorshString counts UTF-8 bytes, not characters" $
            -- "é" is 1 character but 2 UTF-8 bytes (0xC3 0xA9); the u32 prefix
            -- must be the byte length (2), not the char count (1).
            let bs = BL.toStrict (runPut (putBorshString "é"))
                expected = BS.pack [0x02, 0x00, 0x00, 0x00, 0xC3, 0xA9]
             in bs @?= expected,
          testCase "getBorshString decodes u32-prefixed UTF-8" $
            let bs = BS.pack [0x02, 0x00, 0x00, 0x00, 0x61, 0x62]
             in case runGetOrFail getBorshString (BL.fromStrict bs) of
                  Right (_, _, s) -> s @?= "ab"
                  Left (_, _, err) -> assertFailure $ "getBorshString failed: " <> err,
          testCase "getBorshString fails on invalid UTF-8" $
            let bs = BS.pack [0x02, 0x00, 0x00, 0x00, 0xFF, 0xFE]
             in case runGetOrFail getBorshString (BL.fromStrict bs) of
                  Right (_, _, _) -> assertFailure "expected UTF-8 decode failure"
                  Left _ -> pure (),
          QC.testProperty "string round-trip (ASCII)" $ \s ->
            let printable = filter (\c -> c >= ' ' && c <= '~') s
                bs = BL.toStrict (runPut (putBorshString printable))
             in case runGetOrFail getBorshString (BL.fromStrict bs) of
                  Right (_, _, decoded) -> decoded == printable
                  Left _ -> False
        ],
      testGroup
        "option"
        [ testCase "putBorshOption Nothing produces tag 0" $
            let bs = BL.toStrict (runPut (putBorshOption putBorshString Nothing))
             in bs @?= BS.pack [0x00],
          testCase "putBorshOption Just encodes tag 1 + value" $
            let bs = BL.toStrict (runPut (putBorshOption putBorshString (Just "a")))
                -- Tag 1, then string "a": length 1 (u32 LE = 0x01 0x00 0x00 0x00), then 0x61 (a)
                expected = BS.pack [0x01, 0x01, 0x00, 0x00, 0x00, 0x61]
             in bs @?= expected,
          testCase "getBorshOption decodes Nothing (tag 0)" $
            let bs = BS.pack [0x00]
             in case runGetOrFail (getBorshOption getBorshString) (BL.fromStrict bs) of
                  Right (_, _, opt) -> opt @?= Nothing
                  Left (_, _, err) -> assertFailure $ "getBorshOption failed: " <> err,
          testCase "getBorshOption decodes Just (tag 1)" $
            let bs = BS.pack [0x01, 0x01, 0x00, 0x00, 0x00, 0x61]
             in case runGetOrFail (getBorshOption getBorshString) (BL.fromStrict bs) of
                  Right (_, _, opt) -> opt @?= Just "a"
                  Left (_, _, err) -> assertFailure $ "getBorshOption failed: " <> err,
          testCase "getBorshOption fails on invalid tag (>1)" $
            let bs = BS.pack [0x02]
             in case runGetOrFail (getBorshOption getBorshString) (BL.fromStrict bs) of
                  Right (_, _, _) -> assertFailure "expected tag validation failure"
                  Left _ -> pure (),
          QC.testProperty "option round-trip (Nothing)" $
            let bs = BL.toStrict (runPut (putBorshOption putBorshString (Nothing :: Maybe String)))
             in case runGetOrFail (getBorshOption getBorshString) (BL.fromStrict bs) of
                  Right (_, _, opt) -> isNothing opt
                  Left _ -> False,
          QC.testProperty "option round-trip (Just)" $ \s ->
            let printable = filter (\c -> c >= ' ' && c <= '~') s
                bs = BL.toStrict (runPut (putBorshOption putBorshString (Just printable)))
             in case runGetOrFail (getBorshOption getBorshString) (BL.fromStrict bs) of
                  Right (_, _, opt) -> opt == Just printable
                  Left _ -> False
        ],
      testGroup
        "vec"
        [ testCase "putBorshVec empty produces count 0" $
            let bs = BL.toStrict (runPut (putBorshVec putBorshBool []))
             in bs @?= BS.pack [0x00, 0x00, 0x00, 0x00],
          testCase "putBorshVec encodes u32 count + elements" $
            let bs = BL.toStrict (runPut (putBorshVec putBorshBool [True, False]))
                -- Count 2 (u32 LE = 0x02 0x00 0x00 0x00), then True (0x01), False (0x00)
                expected = BS.pack [0x02, 0x00, 0x00, 0x00, 0x01, 0x00]
             in bs @?= expected,
          testCase "getBorshVec decodes empty list" $
            let bs = BS.pack [0x00, 0x00, 0x00, 0x00]
             in case runGetOrFail (getBorshVec getBorshBool) (BL.fromStrict bs) of
                  Right (_, _, xs) -> xs @?= []
                  Left (_, _, err) -> assertFailure $ "getBorshVec failed: " <> err,
          testCase "getBorshVec decodes count + elements" $
            let bs = BS.pack [0x02, 0x00, 0x00, 0x00, 0x01, 0x00]
             in case runGetOrFail (getBorshVec getBorshBool) (BL.fromStrict bs) of
                  Right (_, _, xs) -> xs @?= [True, False]
                  Left (_, _, err) -> assertFailure $ "getBorshVec failed: " <> err,
          QC.testProperty "vec round-trip (bools)" $ \bs ->
            let truncated = take 10 bs -- small list for reasonable test
                putVec = runPut (putBorshVec putBorshBool truncated)
             in case runGetOrFail (getBorshVec getBorshBool) putVec of
                  Right (_, _, decoded) -> decoded == truncated
                  Left _ -> False
        ],
      testGroup
        "bool"
        [ testCase "putBorshBool False produces 0x00" $
            let bs = BL.toStrict (runPut (putBorshBool False))
             in bs @?= BS.pack [0x00],
          testCase "putBorshBool True produces 0x01" $
            let bs = BL.toStrict (runPut (putBorshBool True))
             in bs @?= BS.pack [0x01],
          testCase "getBorshBool decodes False (0x00)" $
            let bs = BS.pack [0x00]
             in case runGetOrFail getBorshBool (BL.fromStrict bs) of
                  Right (_, _, b) -> b @?= False
                  Left (_, _, err) -> assertFailure $ "getBorshBool failed: " <> err,
          testCase "getBorshBool decodes True (0x01)" $
            let bs = BS.pack [0x01]
             in case runGetOrFail getBorshBool (BL.fromStrict bs) of
                  Right (_, _, b) -> b @?= True
                  Left (_, _, err) -> assertFailure $ "getBorshBool failed: " <> err,
          testCase "getBorshBool fails on invalid byte (2)" $
            let bs = BS.pack [0x02]
             in case runGetOrFail getBorshBool (BL.fromStrict bs) of
                  Right (_, _, _) -> assertFailure "expected bool validation failure"
                  Left _ -> pure (),
          testCase "getBorshBool fails on invalid byte (255)" $
            let bs = BS.pack [0xFF]
             in case runGetOrFail getBorshBool (BL.fromStrict bs) of
                  Right (_, _, _) -> assertFailure "expected bool validation failure"
                  Left _ -> pure (),
          QC.testProperty "bool round-trip" $ \b ->
            let bs = BL.toStrict (runPut (putBorshBool b))
             in case runGetOrFail getBorshBool (BL.fromStrict bs) of
                  Right (_, _, decoded) -> decoded == b
                  Left _ -> False
        ]
    ]
