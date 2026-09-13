{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Network.Solana.Core.VersionedMessage
-- Description : v0 message compilation (address lookup tables) and versioned-transaction signing.
module Network.Solana.Core.VersionedMessage
  ( AddressLookupTableAccount (..),
    MessageAddressTableLookup (..),
    compileV0Message,
    compileV0MessageWithPayer,
    newV0TransactionIntent,
    newV0TransactionIntentWithPayer,
  )
where

import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (elemIndex, partition)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Network.Solana.Core.Block (BlockHash)
import Network.Solana.Core.Compact
import Network.Solana.Core.Crypto (SolanaPrivateKey, SolanaPublicKey, dsign, toBase64String)
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message

------------------------------------------------------------------------------------------------

-- * AddressLookupTableAccount

------------------------------------------------------------------------------------------------

-- | An address lookup table as loaded on-chain: its own address, and the
-- ordered list of addresses it stores. Passed to 'compileV0Message' as a
-- candidate source of accounts, so message account keys can be replaced by
-- a lookup index instead of being listed in full.
data AddressLookupTableAccount = AddressLookupTableAccount
  { -- | The lookup table account's own address.
    altKey :: SolanaPublicKey,
    -- | The addresses stored in the table, in on-chain order (a key's
    -- lookup index is its position in this list).
    altAddresses :: [SolanaPublicKey]
  }
  deriving (Show, Eq, Generic)

------------------------------------------------------------------------------------------------

-- * MessageAddressTableLookup

------------------------------------------------------------------------------------------------

-- | A v0 message's reference to one address lookup table: the table's own
-- address, and the indexes within it to load as writable and read-only
-- accounts respectively.
data MessageAddressTableLookup = MessageAddressTableLookup
  { -- | The lookup table account's own address.
    mtlAccountKey :: SolanaPublicKey,
    -- | Indexes of writable accounts to load from the table.
    mtlWritableIndexes :: [Word8],
    -- | Indexes of read-only accounts to load from the table.
    mtlReadonlyIndexes :: [Word8]
  }
  deriving (Show, Eq, Generic)

instance Binary MessageAddressTableLookup where
  put :: MessageAddressTableLookup -> Put
  put MessageAddressTableLookup {..} = do
    put mtlAccountKey
    put (mkCompact mtlWritableIndexes)
    put (mkCompact mtlReadonlyIndexes)
  get :: Get MessageAddressTableLookup
  get =
    MessageAddressTableLookup
      <$> get
      <*> (unCompact <$> get)
      <*> (unCompact <$> get)

------------------------------------------------------------------------------------------------

-- * Compile v0 message

------------------------------------------------------------------------------------------------

-- | Compile a v0 message (mirrors the Rust SDK's @v0::Message::try_compile@):
-- build the legacy message (privilege union + canonical key ordering, via
-- 'mkNewMessage'), then move eligible writable-unsigned and
-- readonly-unsigned keys into the given lookup tables, in table order, with
-- the first table containing a key winning. Program ids and signers never
-- move to a lookup. Tables that end up with no keys loaded are omitted from
-- the serialized message.
--
-- Fails with a 'CompileException' if a table has more than 256 addresses
-- and a key drained into it sits at position 256 or beyond — that position
-- cannot be represented in the byte-sized lookup index the wire format
-- requires, so it is rejected rather than silently truncated (wrapped) into
-- the wrong index, mirroring Rust's @AddressLookupTableIndexOverflow@.
-- Compilation also fails when the static and table-loaded keys together
-- exceed 256, since instruction account indices are single bytes (the
-- on-chain v0 sanitize rule; see 'compileInstruction').
--
-- __A trap this module can't protect you from:__ an address only just added
-- to a lookup table by an @ExtendLookupTable@ instruction that landed in
-- slot @S@ only becomes eligible for lookup in a slot strictly greater than
-- @S@ -- on-chain, the table's @last_extended_slot@ is set to @S@, and its
-- active-address count stays at the length recorded in
-- @last_extended_slot_start_index@ (the table's length before the first
-- extension that landed in @S@ -- relevant if you batch several extends
-- into the same slot) until the current slot passes @S@. The invariant that
-- matters: whatever bank resolves the lookup (a preflight simulation, or
-- the bank that actually processes the transaction) must be at a slot
-- strictly greater than @S@, or the submission is rejected with
-- \"Transaction address table lookup uses an invalid index\" -- not a bug
-- in this function, but a real on-chain rule to wait out. Read @S@ back
-- with 'Network.Solana.NativePrograms.AddressLookupTable.decodeLookupTable'
-- (its 'Network.Solana.NativePrograms.AddressLookupTable.ltLastExtendedSlot'
-- field) if you need to check it explicitly; the convenience wrapper
-- 'Network.Solana.SolanaWeb3.getLookupTable' drops that field and returns
-- only 'AddressLookupTableAccount', so it can't be used for this check.
compileV0Message :: BlockHash -> [Instruction] -> [AddressLookupTableAccount] -> Either CompileException BS.ByteString
compileV0Message bh instructions = compileV0FromLegacy (mkNewMessage bh instructions)

-- | Like 'compileV0Message', but with an explicitly named fee payer, seeded
-- as a writable signer and pinned to account 0 via
-- 'Network.Solana.Core.Message.mkNewMessageWithPayer' -- which is what the
-- Rust SDK's @v0::Message::try_compile(payer, ..)@ always does. Required
-- when no instruction marks a signer writable (a memo-only or SPL
-- authority-only instruction list) and for sponsored fees.
compileV0MessageWithPayer :: SolanaPublicKey -> BlockHash -> [Instruction] -> [AddressLookupTableAccount] -> Either CompileException BS.ByteString
compileV0MessageWithPayer payer bh instructions = compileV0FromLegacy (mkNewMessageWithPayer payer bh instructions)

-- | Move eligible keys of an assembled legacy message into the given lookup
-- tables and serialize the v0 message (shared body of 'compileV0Message'
-- and 'compileV0MessageWithPayer').
compileV0FromLegacy :: Message -> [AddressLookupTableAccount] -> Either CompileException BS.ByteString
compileV0FromLegacy legacy tables = do
  let header = mHeader legacy
      keys = mAccountKeys legacy
      (rws, ros, rwus, rous) = splitAccountsByPurpose header keys
      programIds = iProgramId <$> mInstructions legacy
      (rwusRemaining, rousRemaining, tableLoads) = drainTables programIds rwus rous tables
      statics = rws <> ros <> rwusRemaining <> rousRemaining
      newHeader = header {numReadonlyUnsignedAccounts = fromIntegral (length rousRemaining)}
      resolutionKeys =
        statics
          <> concatMap tlWritableKeys tableLoads
          <> concatMap tlReadonlyKeys tableLoads
  byteIndexes <- mapM toByteIndexes tableLoads
  let lookups =
        [ MessageAddressTableLookup (altKey (tlTable tl)) writableIdx readonlyIdx
          | (tl, (writableIdx, readonlyIdx)) <- zip tableLoads byteIndexes,
            not (null writableIdx && null readonlyIdx)
        ]
  compiledInstructions <- mapM (compileInstruction resolutionKeys) (mInstructions legacy)
  return . BL.toStrict . runPut $ do
    putWord8 0x80
    put newHeader
    put (mkCompact statics)
    put (mRecentBlockhash legacy)
    put (mkCompact compiledInstructions)
    put (mkCompact lookups)

-- | Sign a v0-compiled message with each signer's private key, and return
-- the Base64-encoded signed versioned transaction (compact signature array
-- followed by the message bytes). Mirrors 'Network.Solana.Core.Message.newTransactionIntent'.
-- The private keys passed here must be ordered to match the resulting account
-- order (fee payer first), otherwise the produced signatures will not correspond
-- to the right account keys. If you would rather name the fee payer explicitly
-- and not worry about signer order, use 'newV0TransactionIntentWithPayer'.
--
-- If any given table was extended very recently, see the note on
-- 'compileV0Message' about the resulting transaction's activation window:
-- whatever bank resolves the lookup must be at a slot strictly past the
-- table's last extension, regardless of the preflight commitment level used
-- to submit it (and regardless of preflight at all, if it is skipped).
newV0TransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> [AddressLookupTableAccount] -> BlockHash -> Either CompileException String
newV0TransactionIntent signers instructions tables bh = do
  msg <- compileV0Message bh instructions tables
  let signatures = BL.toStrict . encode $ mkCompact $ flip dsign msg <$> signers
  return $ toBase64String $ BS.append signatures msg

-- | Like 'newV0TransactionIntent', but with an explicitly named fee payer
-- (see 'compileV0MessageWithPayer'); the given private keys may be listed
-- in any order -- they are matched against the message's required signers
-- and placed in message order automatically, with the same 'Left' cases as
-- 'Network.Solana.Core.Message.newTransactionIntentWithPayer'.
newV0TransactionIntentWithPayer :: SolanaPublicKey -> [SolanaPrivateKey] -> [Instruction] -> [AddressLookupTableAccount] -> BlockHash -> Either CompileException String
newV0TransactionIntentWithPayer payer signingKeys instructions tables bh = do
  let legacy = mkNewMessageWithPayer payer bh instructions
  msg <- compileV0FromLegacy legacy tables
  orderedKeys <- orderSigningKeys legacy signingKeys
  let signatures = BL.toStrict . encode $ mkCompact $ flip dsign msg <$> orderedKeys
  return $ toBase64String $ BS.append signatures msg

------------------------------------------------------------------------------------------------

-- * Internal helpers

------------------------------------------------------------------------------------------------

-- | The keys drained from the static writable-/readonly-unsigned sections
-- into one lookup table, alongside the resulting lookup positions. Positions
-- are kept as plain 'Int' (not yet 'Word8') since a table may have more than
-- 256 addresses; 'toByteIndexes' validates and narrows them.
data TableLoad = TableLoad
  { tlTable :: AddressLookupTableAccount,
    tlWritableKeys :: [SolanaPublicKey],
    tlReadonlyKeys :: [SolanaPublicKey],
    tlWritableIdx :: [Int],
    tlReadonlyIdx :: [Int]
  }

-- | Drain eligible keys (present in a table's addresses, and not a program
-- id) from the writable-unsigned and readonly-unsigned pools into each
-- table in turn, preserving pool order within each table and leaving
-- already-drained keys unavailable to later tables. Returns the remaining
-- (undrained) pools and the per-table loads, in table order.
drainTables ::
  [SolanaPublicKey] ->
  [SolanaPublicKey] ->
  [SolanaPublicKey] ->
  [AddressLookupTableAccount] ->
  ([SolanaPublicKey], [SolanaPublicKey], [TableLoad])
drainTables _ rwus rous [] = (rwus, rous, [])
drainTables programIds rwus rous (table : rest) =
  let addrs = altAddresses table
      eligible k = k `notElem` programIds && k `elem` addrs
      (wDrain, rwus') = partition eligible rwus
      (rDrain, rous') = partition eligible rous
      indexIn k = fromJust (elemIndex k addrs)
      tableLoad =
        TableLoad
          { tlTable = table,
            tlWritableKeys = wDrain,
            tlReadonlyKeys = rDrain,
            tlWritableIdx = indexIn <$> wDrain,
            tlReadonlyIdx = indexIn <$> rDrain
          }
      (rwusFinal, rousFinal, restLoads) = drainTables programIds rwus' rous' rest
   in (rwusFinal, rousFinal, tableLoad : restLoads)

-- | Narrow one table's drained positions to the 'Word8' indexes the
-- serialized lookup format requires, failing if any position doesn't fit in
-- a byte.
toByteIndexes :: TableLoad -> Either CompileException ([Word8], [Word8])
toByteIndexes tl
  | all fitsInByte (tlWritableIdx tl <> tlReadonlyIdx tl) =
      Right (fromIntegral <$> tlWritableIdx tl, fromIntegral <$> tlReadonlyIdx tl)
  | otherwise =
      Left
        ( MissingIndex
            ( "address table lookup index overflow: table "
                <> show (altKey (tlTable tl))
                <> " has more than 256 addresses"
            )
        )
  where
    fitsInByte i = i <= fromIntegral (maxBound :: Word8)

-- | Split a message's account keys into its four privilege sections
-- (writable-signed, readonly-signed, writable-unsigned, readonly-unsigned)
-- using the header's counts. Recomputed locally: 'Message'\'s equivalent
-- helper is not exported.
splitAccountsByPurpose ::
  MessageHeader ->
  [SolanaPublicKey] ->
  ([SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey])
splitAccountsByPurpose (MessageHeader numRequiredSignatures' numReadonlySigned' numReadonlyUnsigned') keys =
  let numRequiredSignatures = fromIntegral numRequiredSignatures'
      numReadonlySigned = fromIntegral numReadonlySigned'
      numReadonlyUnsigned = fromIntegral numReadonlyUnsigned'
      (signed, unsigned) = splitAt numRequiredSignatures keys
      (readAndWriteSigned, readOnlySigned) = splitAt (numRequiredSignatures - numReadonlySigned) signed
      unsignedCount = length unsigned
      (readAndWriteUnsigned, readOnlyUnsigned) = splitAt (unsignedCount - numReadonlyUnsigned) unsigned
   in (readAndWriteSigned, readOnlySigned, readAndWriteUnsigned, readOnlyUnsigned)
