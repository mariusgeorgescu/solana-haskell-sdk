{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Address Lookup Table program. Instruction data is
-- bincode-encoded (u32 little-endian discriminant), like the System
-- Program; the @ExtendLookupTable@ variant's address vector is bincode's
-- @Vec\<Pubkey\>@ encoding (u64 little-endian count followed by the raw
-- 32-byte keys).
module Network.Solana.NativePrograms.AddressLookupTable where

import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Pda (findProgramAddress)
import Network.Solana.Core.VersionedMessage qualified as VM
import Network.Solana.NativePrograms.SystemProgram qualified as SP

-- | Address Lookup Table program address.
addressLookupTableProgramId :: SolanaPublicKey
addressLookupTableProgramId = "AddressLookupTab1e1111111111111111111111111"

-- | Address Lookup Table program instructions.
data AddressLookupTableInstruction
  = CreateLookupTable {recentSlot :: Word64, bumpSeed :: Word8}
  | FreezeLookupTable
  | ExtendLookupTable {newAddresses :: [SolanaPublicKey]}
  | DeactivateLookupTable
  | CloseLookupTable
  deriving (Eq, Show, Generic)

-- bincode encodes a Rust Vec<Pubkey> as a u64 little-endian count followed
-- by the raw 32-byte keys.
putBincodeVec :: [SolanaPublicKey] -> Put
putBincodeVec pks = do
  putWord64le (fromIntegral (length pks))
  mapM_ (putByteString . getSolanaPublicKeyRaw) pks

-- | bincode decodes a Rust Vec<Pubkey> as a u64 little-endian count
-- followed by that many raw 32-byte keys.
getBincodeVec :: Get [SolanaPublicKey]
getBincodeVec = do
  count <- getWord64le
  if count > fromIntegral (maxBound :: Int)
    then fail "getBincodeVec: count exceeds Int range"
    else replicateM (fromIntegral count) get

instance Binary AddressLookupTableInstruction where
  put :: AddressLookupTableInstruction -> Put
  put (CreateLookupTable slot bump) = do
    putWord32le 0
    putWord64le slot
    putWord8 bump
  put FreezeLookupTable = putWord32le 1
  put (ExtendLookupTable addrs) = do
    putWord32le 2
    putBincodeVec addrs
  put DeactivateLookupTable = putWord32le 3
  put CloseLookupTable = putWord32le 4

  get :: Get AddressLookupTableInstruction
  get = do
    disc <- getWord32le
    case disc of
      0 -> CreateLookupTable <$> getWord64le <*> getWord8
      1 -> pure FreezeLookupTable
      2 -> ExtendLookupTable <$> getBincodeVec
      3 -> pure DeactivateLookupTable
      4 -> pure CloseLookupTable
      _ -> fail ("AddressLookupTableInstruction: unknown discriminant " <> show disc)

-- | An address lookup table account's on-chain state: the fixed-size
-- @LookupTableMeta@ fields followed by the raw stored addresses.
data LookupTableState = LookupTableState
  { ltDeactivationSlot :: Word64,
    ltLastExtendedSlot :: Word64,
    ltLastExtendedSlotStartIndex :: Word8,
    ltAuthority :: Maybe SolanaPublicKey,
    ltAddresses :: [SolanaPublicKey]
  }
  deriving (Eq, Show)

-- | Decodes an address lookup table account's data. The first 56 bytes are
-- the fixed @LookupTableMeta@ region (u32 discriminant, two u64 slots, a u8
-- start index, a bincode @Option\<Pubkey\>@ authority, and trailing padding
-- up to the 56-byte boundary); the remainder is the raw table of 32-byte
-- addresses.
decodeLookupTable :: BS.ByteString -> Either String LookupTableState
decodeLookupTable bs
  | BS.length bs < 56 =
      Left ("LookupTableState: expected at least 56 bytes, got " <> show (BS.length bs))
  | otherwise = do
      (deactivationSlot, lastExtendedSlot, startIndex, authority) <- parseMeta (BS.take 56 bs)
      addresses <- parseAddresses (BS.drop 56 bs)
      Right (LookupTableState deactivationSlot lastExtendedSlot startIndex authority addresses)
  where
    parseMeta chunk = case runGetOrFail getMeta (BL.fromStrict chunk) of
      Left (_, _, err) -> Left err
      Right (_, _, v) -> Right v
    getMeta = do
      disc <- getWord32le
      case disc of
        0 -> fail "uninitialized lookup table"
        1 ->
          (,,,)
            <$> getWord64le
            <*> getWord64le
            <*> getWord8
            <*> getOptionalAuthority
        _ -> fail ("LookupTableState: unknown discriminant " <> show disc)
    getOptionalAuthority = do
      tag <- getWord8
      case tag of
        0 -> pure Nothing
        1 -> Just <$> get
        _ -> fail ("LookupTableState: invalid authority option tag " <> show tag)
    parseAddresses addrBytes
      | remainder /= 0 =
          Left ("LookupTableState: addresses length " <> show (BS.length addrBytes) <> " is not a multiple of 32")
      | otherwise = case runGetOrFail (replicateM count get) (BL.fromStrict addrBytes) of
          Left (_, _, err) -> Left err
          Right (_, _, addrs) -> Right addrs
      where
        (count, remainder) = BS.length addrBytes `divMod` 32

-- | Builds the 'VM.AddressLookupTableAccount' 'compileV0Message' expects from
-- the table's own address and its decoded on-chain state.
lookupTableToAccount :: SolanaPublicKey -> LookupTableState -> VM.AddressLookupTableAccount
lookupTableToAccount key state = VM.AddressLookupTableAccount key (ltAddresses state)

-- | Derives the address of a lookup table for a given authority and recent
-- slot: the PDA of @[authority, recentSlot as 8 little-endian bytes]@ under
-- the Address Lookup Table program.
deriveLookupTableAddress :: SolanaPublicKey -> Word64 -> Maybe (SolanaPublicKey, Word8)
deriveLookupTableAddress authority slot =
  findProgramAddress
    [ getSolanaPublicKeyRaw authority,
      BL.toStrict (runPut (putWord64le slot))
    ]
    addressLookupTableProgramId

-- | Creates instruction to "Create a lookup table" and returns it together
-- with the table's derived address.
-- Receives the authority (not required to sign), the payer (which must
-- sign) and the recent slot used to derive the table's address.
-- Calls 'error' if the lookup-table address cannot be derived (practically
-- unreachable: requires every bump candidate to land on-curve).
-- # Account references
-- 0. `[WRITE]` Uninitialized lookup table account
-- 1. `[]` Authority
-- 2. `[WRITE, SIGNER]` Payer account
-- 3. `[]` System program
createLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Word64 -> (Instruction, SolanaPublicKey)
createLookupTable authority payer slot =
  let (table, bump) =
        fromMaybe
          (error "deriveLookupTableAddress: derivation failed")
          (deriveLookupTableAddress authority slot)
      ix =
        mkInstruction
          addressLookupTableProgramId
          [ AccountMeta {accountPubKey = table, isSigner = False, isWritable = True},
            AccountMeta {accountPubKey = authority, isSigner = False, isWritable = False},
            AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True},
            AccountMeta {accountPubKey = SP.systemProgramId, isSigner = False, isWritable = False}
          ]
          (CreateLookupTable slot bump)
   in (ix, table)

-- | Creates instruction to "Freeze a lookup table, making it immutable"
-- Receives the lookup table account and its authority (which must sign).
-- # Account references
-- 0. `[WRITE]` Lookup table account
-- 1. `[SIGNER]` Authority
freezeLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Instruction
freezeLookupTable table authority =
  mkInstruction
    addressLookupTableProgramId
    [ AccountMeta {accountPubKey = table, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
    ]
    FreezeLookupTable

-- | Creates instruction to "Extend a lookup table with new addresses"
-- Receives the lookup table account, its authority (which must sign), an
-- optional payer to fund the extension (the system program is always
-- included alongside it) and the addresses to append.
-- # Account references
-- 0. `[WRITE]` Lookup table account
-- 1. `[SIGNER]` Authority
-- 2. `[WRITE, SIGNER]` (optional) Payer account
-- 3. `[]` (optional) System program
extendLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey -> [SolanaPublicKey] -> Instruction
extendLookupTable table authority mPayer addrs =
  mkInstruction
    addressLookupTableProgramId
    ( [ AccountMeta {accountPubKey = table, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
      ]
        ++ maybe
          []
          ( \payer ->
              [ AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True},
                AccountMeta {accountPubKey = SP.systemProgramId, isSigner = False, isWritable = False}
              ]
          )
          mPayer
    )
    (ExtendLookupTable addrs)

-- | Creates instruction to "Deactivate a lookup table, making it unusable
-- and eligible for closure once it is no longer referenced by any
-- transaction"
-- Receives the lookup table account and its authority (which must sign).
-- # Account references
-- 0. `[WRITE]` Lookup table account
-- 1. `[SIGNER]` Authority
deactivateLookupTable :: SolanaPublicKey -> SolanaPublicKey -> Instruction
deactivateLookupTable table authority =
  mkInstruction
    addressLookupTableProgramId
    [ AccountMeta {accountPubKey = table, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False}
    ]
    DeactivateLookupTable

-- | Creates instruction to "Close a deactivated lookup table, reclaiming
-- its lamports"
-- Receives the lookup table account, its authority (which must sign) and
-- the recipient of the reclaimed lamports.
-- # Account references
-- 0. `[WRITE]` Lookup table account
-- 1. `[SIGNER]` Authority
-- 2. `[WRITE]` Recipient account
closeLookupTable :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
closeLookupTable table authority recipient =
  mkInstruction
    addressLookupTableProgramId
    [ AccountMeta {accountPubKey = table, isSigner = False, isWritable = True},
      AccountMeta {accountPubKey = authority, isSigner = True, isWritable = False},
      AccountMeta {accountPubKey = recipient, isSigner = False, isWritable = True}
    ]
    CloseLookupTable
