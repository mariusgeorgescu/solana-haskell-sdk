-- |
-- Module      : Network.Solana.Core.Borsh
-- Description : Borsh serialization helpers for common types.
--
-- This module provides helpers for Borsh serialization, which differs from Bincode
-- in specific length encodings:
--
-- * __String lengths__: Borsh uses 32-bit LE (u32), Bincode uses 64-bit LE (u64).
-- * __Vec counts__: Borsh uses 32-bit LE (u32), Bincode uses 64-bit LE (u64).
-- * __Option tags__: Both use 8-bit (u8) tags; Borsh enforces strict 0/1 validation.
-- * __Bool encoding__: Both use 8-bit (u8); 0 = False, 1 = True; Borsh enforces strict validation.
module Network.Solana.Core.Borsh
  ( putBorshString,
    getBorshString,
    putBorshOption,
    getBorshOption,
    putBorshVec,
    getBorshVec,
    putBorshBool,
    getBorshBool,
  )
where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getByteString, getWord32le, getWord8)
import Data.Binary.Put (Put, putByteString, putWord32le, putWord8)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | Serialize a 'String' in Borsh format: u32 LE byte-length + UTF-8 bytes.
-- The length prefix is a 32-bit little-endian word (u32).
putBorshString :: String -> Put
putBorshString s = do
  let bs = TE.encodeUtf8 (T.pack s)
  putWord32le (fromIntegral (BS.length bs))
  putByteString bs

-- | Deserialize a 'String' in Borsh format: u32 LE byte-length + UTF-8 bytes.
-- Fails if the length exceeds @maxBound :: Int@ or if the bytes are not valid UTF-8.
getBorshString :: Get String
getBorshString = do
  len <- getWord32le
  if len > fromIntegral (maxBound :: Int)
    then fail "getBorshString: length exceeds Int range"
    else do
      bs <- getByteString (fromIntegral len)
      case TE.decodeUtf8' bs of
        Left _ -> fail "getBorshString: invalid UTF-8"
        Right t -> pure (T.unpack t)

-- | Serialize a 'Maybe' value in Borsh format: u8 tag (0 = Nothing, 1 = Just) + optional value.
putBorshOption :: (a -> Put) -> Maybe a -> Put
putBorshOption _ Nothing = putWord8 0
putBorshOption putVal (Just a) = do
  putWord8 1
  putVal a

-- | Deserialize a 'Maybe' value in Borsh format: u8 tag (0 = Nothing, 1 = Just) + optional value.
-- Fails if the tag is not 0 or 1.
getBorshOption :: Get a -> Get (Maybe a)
getBorshOption getVal = do
  tag <- getWord8
  case tag of
    0 -> pure Nothing
    1 -> Just <$> getVal
    _ -> fail $ "getBorshOption: invalid tag " <> show tag <> " (expected 0 or 1)"

-- | Serialize a list in Borsh format: u32 LE count + elements.
putBorshVec :: (a -> Put) -> [a] -> Put
putBorshVec putVal xs = do
  putWord32le (fromIntegral (length xs))
  mapM_ putVal xs

-- | Deserialize a list in Borsh format: u32 LE count + elements.
-- Fails if the count exceeds @maxBound :: Int@.
getBorshVec :: Get a -> Get [a]
getBorshVec getVal = do
  count <- getWord32le
  if count > fromIntegral (maxBound :: Int)
    then fail "getBorshVec: count exceeds Int range"
    else replicateM (fromIntegral count) getVal

-- | Serialize a 'Bool' in Borsh format: u8 (0 = False, 1 = True).
putBorshBool :: Bool -> Put
putBorshBool False = putWord8 0
putBorshBool True = putWord8 1

-- | Deserialize a 'Bool' in Borsh format: u8 (0 = False, 1 = True).
-- Fails if the byte is not 0 or 1.
getBorshBool :: Get Bool
getBorshBool = do
  b <- getWord8
  case b of
    0 -> pure False
    1 -> pure True
    _ -> fail $ "getBorshBool: invalid byte " <> show b <> " (expected 0 or 1)"
