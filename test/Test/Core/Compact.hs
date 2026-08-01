module Test.Core.Compact (tests) where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word8)
import Network.Solana.Core.Compact
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "compact-u16"
    [ testCase "known encodings" $ do
        BL.unpack (encodeCompactU16 0) @?= [0x00]
        BL.unpack (encodeCompactU16 1) @?= [0x01]
        BL.unpack (encodeCompactU16 127) @?= [0x7f]
        BL.unpack (encodeCompactU16 128) @?= [0x80, 0x01]
        BL.unpack (encodeCompactU16 16383) @?= [0xff, 0x7f]
        BL.unpack (encodeCompactU16 16384) @?= [0x80, 0x80, 0x01]
        BL.unpack (encodeCompactU16 65535) @?= [0xff, 0xff, 0x03],
      testProperty "decode . encode == id" $ \w ->
        decodeCompactU16 (encodeCompactU16 w) === Right w,
      testProperty "encoded length matches value range" $ \w ->
        let len = fromIntegral (BL.length (encodeCompactU16 w)) :: Int
         in if w < 0x80
              then len === 1
              else
                if w < 0x4000
                  then len === 2
                  else len === 3,
      testCase "rejects overlong encoding" $
        assertLeft (decodeCompactU16 (BL.pack [0x80, 0x80, 0x80, 0x01])),
      testCase "rejects aliased encoding" $
        assertLeft (decodeCompactU16 (BL.pack [0x80, 0x00])),
      testProperty "CompactArray Binary decode . encode == id" $
        forAll (resize 500 (listOf arbitrary)) $ \(xs :: [Word8]) ->
          let ca = mkCompact xs in decode (encode ca) === ca
    ]

assertLeft :: (Show b) => Either a b -> Assertion
assertLeft (Left _) = pure ()
assertLeft (Right v) = assertFailure ("expected decode failure, got " <> show v)
