{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Network.Solana.Core.VersionedMessage
-- Description : v0 message compilation (address lookup tables) and versioned-transaction signing.
module Network.Solana.Core.VersionedMessage
  ( AddressLookupTableAccount (..),
    MessageAddressTableLookup (..),
    compileV0Message,
    newV0TransactionIntent,
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
--
-- __A trap this module can't protect you from:__ an address only just added
-- to a lookup table by an @ExtendLookupTable@ instruction that landed in
-- slot @S@ only becomes eligible for lookup in a slot strictly greater than
-- @S@ (on-chain, the table's @last_extended_slot@ is set to @S@ and its
-- active-address count is the pre-extension length until the current slot
-- passes it). This is invisible from the client side: even once the
-- extending transaction has reached @finalized@ status and
-- 'Network.Solana.SolanaWeb3.getLookupTable' reads back the extended
-- addresses in full (it only exposes 'AddressLookupTableAccount', not the
-- on-chain @last_extended_slot@ that would let you check this), a
-- transaction compiled here and submitted for @finalized@-commitment
-- preflight can still be simulated against a bank sitting at exactly @S@,
-- and rejected with \"Transaction address table lookup uses an invalid
-- index\" -- not a bug in this function, but worth knowing before you build
-- a v0 transaction immediately after extending the table it references. A
-- send preflighted at @confirmed@ commitment (closer to the tip) avoids it.
compileV0Message :: BlockHash -> [Instruction] -> [AddressLookupTableAccount] -> Either CompileException BS.ByteString
compileV0Message bh instructions tables = do
  let legacy = mkNewMessage bh instructions
      header = mHeader legacy
      keys = mAccountKeys legacy
      (rws, ros, rwus, rous) = splitAccountsByPurpose header keys
      programIds = iProgramId <$> instructions
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
-- to the right account keys.
--
-- If any given table was extended very recently, see the note on
-- 'compileV0Message' about the resulting transaction's activation window
-- before submitting it with a @finalized@-commitment preflight.
newV0TransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> [AddressLookupTableAccount] -> BlockHash -> Either CompileException String
newV0TransactionIntent signers instructions tables bh = do
  msg <- compileV0Message bh instructions tables
  let signatures = BL.toStrict . encode $ mkCompact $ flip dsign msg <$> signers
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
