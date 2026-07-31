{-# LANGUAGE OverloadedStrings #-}

module Network.Solana.SplPrograms.Memo where

import Data.Binary
import Data.Binary.Get (getRemainingLazyByteString)
import Data.Binary.Put (putByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.Core.Instruction

-- | SPL Memo program (v2) address. The program validates a string of UTF-8
-- encoded characters and verifies that any accounts provided are signers of
-- the transaction.
memoProgramId :: SolanaPublicKey
memoProgramId = "MemoSq4gqABAXKb96qnH8TysNcWxMyWCqXgDLGmfcHr"

-- | Memo instruction data: the raw UTF-8 bytes of the memo text
-- (no discriminant, no length prefix). The 'Binary' 'get' decodes leniently
-- by design: invalid UTF-8 sequences are replaced (never fails), and all
-- remaining input is consumed as the memo text. Encoding via 'put' is the
-- production path used when building instructions; decoding exists mainly
-- for round-tripping and inspection.
newtype MemoData = MemoData String
  deriving (Eq, Show)

instance Binary MemoData where
  put :: MemoData -> Put
  put (MemoData s) = putByteString (TE.encodeUtf8 (T.pack s))
  get :: Get MemoData
  get = MemoData . T.unpack . TE.decodeUtf8Lenient . BL.toStrict <$> getRemainingLazyByteString

-- | Creates a memo instruction from the memo text and the list of signer accounts
-- (may be empty). The memo program verifies each provided account signed the transaction.
-- # Account references
-- 0..n. `[SIGNER]` Signer accounts
buildMemo :: String -> [SolanaPublicKey] -> Instruction
buildMemo text signers =
  mkInstruction
    memoProgramId
    [AccountMeta {accountPubKey = s, isSigner = True, isWritable = False} | s <- signers]
    (MemoData text)
