{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Solana.Core.Compact
-- Description : compact-u16 (ShortU16) encoding and compact arrays.
--
-- Solana serializes lengths as /compact-u16/: a 'Word16' packed into 1–3
-- bytes, little-endian, 7 data bits per byte, with the high bit of each
-- byte flagging a continuation byte. The third byte may only carry 2 data
-- bits, and aliased (non-canonical) encodings are rejected when decoding.
-- A 'CompactArray' is a sequence serialized as its compact-u16 length
-- followed by the serialized items.
module Network.Solana.Core.Compact
  ( getCompactU16,
    putCompactU16,
    encodeCompactU16,
    decodeCompactU16,
    CompactArray (),
    mkCompact,
    unCompact,
    getCompactArrayLength,
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

-- | A list paired with its length, serialized ('Binary' 'put') as a
-- compact-u16 length prefix followed by each item. Build with 'mkCompact'.
data CompactArray a = CompactArray
  { -- | The recorded number of items.
    getCompactArrayLength :: Word16,
    -- | The items of the array.
    unCompact :: [a]
  }
  deriving (Eq, Ord, Show, Generic)

-- instance (ToJSON a) => ToJSON (CompactArray a) where
--   toJSON :: CompactArray a -> Value
--   toJSON (CompactArray _ xs) = toJSON xs

-- instance (FromJSON a) => FromJSON (CompactArray a) where
--   parseJSON :: (FromJSON a) => Value -> Parser (CompactArray a)
--   parseJSON v = mkCompact <$> parseJSON v

instance (Binary a) => Binary (CompactArray a) where
  put :: (Binary a) => CompactArray a -> Put
  put (CompactArray i xs) = do
    putCompactU16 i
    mapM_ put xs -- not default putList

-- | Wrap a list into a 'CompactArray', recording its length. The length is
-- truncated to 'Word16'.
mkCompact :: [a] -> CompactArray a
mkCompact xs = CompactArray (fromIntegral $ length xs) xs

------------------------------------------------------------------------------------------------

-- * CompactU16

------------------------------------------------------------------------------------------------

-- | Decode a compact-u16 value. Fails on encodings longer than 3 bytes, on
-- aliased (non-canonical) encodings, on an invalid third byte, and on
-- values exceeding 'Word16'.
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
              if byteIndex > 0 && byte == 0
                then fail "Aliased (non-canonical) compact-u16 encoding"
                else
                  if byteIndex == 2 && (byte .&. 0xFC) /= 0
                    then fail "Invalid 3rd byte in compact-u16 (only 2 bits allowed)"
                    else case fromIntegral acc' of
                      w | w <= 0xFFFF -> return w
                      _ -> fail "Decoded value exceeds u16 range"

-- | Encode a 'Word16' as compact-u16 (1–3 bytes).
putCompactU16 :: Word16 -> Put
putCompactU16 val = go (fromIntegral val :: Word32)
  where
    go :: Word32 -> Put
    go v
      | v < 0x80 = putWord8 (fromIntegral v)
      | otherwise = do
          putWord8 (fromIntegral (v .&. 0x7F) .|. 0x80)
          go (v `shiftR` 7)

-- | Run 'putCompactU16' to a lazy byte string.
encodeCompactU16 :: Word16 -> BL.ByteString
encodeCompactU16 = runPut . putCompactU16

-- | Run 'getCompactU16' over a lazy byte string, returning the decoder's
-- error message on failure.
decodeCompactU16 :: BL.ByteString -> Either String Word16
decodeCompactU16 bs =
  case runGetOrFail getCompactU16 bs of
    Left (_, _, err) -> Left err
    Right (_, _, val) -> Right val
