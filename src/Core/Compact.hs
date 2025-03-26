{-# LANGUAGE OverloadedStrings #-}

{-
Recap rapid: cum funcționează ShortU16
Codifică un u16 (0–65535) folosind între 1 și 3 bytes.

Fiecare byte are un bit de continuare (MSB = 1 înseamnă că urmează alt byte).

Fiecare byte contribuie cu 7 biți de date, începând cu cei mai puțin semnificativi (little-endian).

Al treilea byte (dacă există) poate conține doar 2 biți de date, iar ceilalți 6 trebuie să fie 0.

Se interzic alias-urile (de exemplu, codificări valide, dar redundante, pentru aceeași valoare).

-}
module Core.Compact
  ( getCompactU16,
    putCompactU16,
    encodeCompactU16,
    decodeCompactU16,
    CompactArray,
    mkCompact,
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BL
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * CompactArray

------------------------------------------------------------------------------------------------

data CompactArray a = CompactArray Word16 [a]
  deriving (Eq, Ord, Show, Generic)

instance (Binary a) => Binary (CompactArray a) where
  put :: (Binary a) => CompactArray a -> Put
  put (CompactArray i xs) = do
    putCompactU16 i
    mapM_ put xs -- not default putList


mkCompact :: [a] -> CompactArray a
mkCompact xs = CompactArray (fromIntegral $ length xs) xs

------------------------------------------------------------------------------------------------

-- * CompactU16

------------------------------------------------------------------------------------------------

-- Decode a compact-u16 encoded value
getCompactU16 :: Get Word16
getCompactU16 = go 0 0
  where
    go :: Word32 -> Int -> Get Word16
    go acc byteIndex
      | byteIndex >= 3 = fail "Too many bytes in compact-u16"
      | otherwise = do
          byte <- getWord8
          let value = fromIntegral (byte .&. 0x7F) --
              acc' = acc .|. (value `shiftL` (7 * byteIndex))
              continue = (byte .&. 0x80) /= 0
          if continue
            then go acc' (byteIndex + 1)
            else
              if byteIndex == 2 && (byte .&. 0xFC) /= 0
                then fail "Invalid 3rd byte in compact-u16 (only 2 bits allowed)"
                else case fromIntegral acc' of
                  w | w <= 0xFFFF -> return w
                  _ -> fail "Decoded value exceeds u16 range"

-- Encode a Word16 to compact-u16
putCompactU16 :: Word16 -> Put
putCompactU16 val = go (fromIntegral val :: Word32)
  where
    go :: Word32 -> Put
    go v
      | v < 0x80 = putWord8 (fromIntegral v)
      | otherwise = do
          putWord8 (fromIntegral (v .&. 0x7F) .|. 0x80)
          go (v `shiftR` 7)

-- Convenient helpers
encodeCompactU16 :: Word16 -> BL.ByteString
encodeCompactU16 = runPut . putCompactU16

decodeCompactU16 :: BL.ByteString -> Either String Word16
decodeCompactU16 bs =
  case runGetOrFail getCompactU16 bs of
    Left (_, _, err) -> Left err
    Right (_, _, val) -> Right val
