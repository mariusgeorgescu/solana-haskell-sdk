{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Compute Budget program: instructions setting a
-- transaction's compute-unit limit, compute-unit price (priority fee), heap
-- size and loaded-accounts data size limit. Instruction data is
-- byte-verified against the Rust SDK.
module Network.Solana.NativePrograms.ComputeBudget where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import GHC.Generics (Generic)
import Network.Solana.Core.Crypto (SolanaPublicKey)
import Network.Solana.Core.Instruction

-- | Compute Budget program address. This program contains instructions to
-- set transaction-wide compute limits, heap size and a compute unit price
-- ("priority fee") for transaction prioritization.
computeBudgetProgramId :: SolanaPublicKey
computeBudgetProgramId = "ComputeBudget111111111111111111111111111111"

-- | Compute Budget instructions, serialized in the Rust SDK's borsh layout:
-- a single-byte discriminant followed by the little-endian field.
-- The deprecated variant 0 is intentionally not modeled.
data ComputeBudgetInstruction
  = -- | Request a specific transaction-wide program heap region size in bytes
    -- (must be a multiple of 1024)
    RequestHeapFrame Word32
  | -- | Set a specific compute unit limit that the transaction is allowed to consume
    SetComputeUnitLimit Word32
  | -- | Set a compute unit price in micro-lamports to pay a higher transaction
    -- fee for higher transaction prioritization
    SetComputeUnitPrice Word64
  | -- | Set a specific transaction-wide account data size limit, in bytes, allowed to load
    SetLoadedAccountsDataSizeLimit Word32
  deriving (Eq, Show, Generic)

instance Binary ComputeBudgetInstruction where
  put :: ComputeBudgetInstruction -> Put
  put (RequestHeapFrame bytes) = do
    putWord8 1
    putWord32le bytes
  put (SetComputeUnitLimit units) = do
    putWord8 2
    putWord32le units
  put (SetComputeUnitPrice microLamports) = do
    putWord8 3
    putWord64le microLamports
  put (SetLoadedAccountsDataSizeLimit bytes) = do
    putWord8 4
    putWord32le bytes

  get :: Get ComputeBudgetInstruction
  get = do
    disc <- getWord8
    case disc of
      1 -> RequestHeapFrame <$> getWord32le
      2 -> SetComputeUnitLimit <$> getWord32le
      3 -> SetComputeUnitPrice <$> getWord64le
      4 -> SetLoadedAccountsDataSizeLimit <$> getWord32le
      _ -> fail ("ComputeBudgetInstruction: unknown discriminant " <> show disc)

-- | Creates instruction to "Request a transaction-wide program heap region size in bytes".
-- Compute Budget instructions take no accounts.
requestHeapFrame :: Word32 -> Instruction
requestHeapFrame bytes = mkInstruction computeBudgetProgramId [] (RequestHeapFrame bytes)

-- | Creates instruction to "Set the transaction's compute unit limit".
-- Compute Budget instructions take no accounts.
setComputeUnitLimit :: Word32 -> Instruction
setComputeUnitLimit units = mkInstruction computeBudgetProgramId [] (SetComputeUnitLimit units)

-- | Creates instruction to "Set the compute unit price in micro-lamports" (priority fee).
-- Compute Budget instructions take no accounts.
setComputeUnitPrice :: Word64 -> Instruction
setComputeUnitPrice microLamports = mkInstruction computeBudgetProgramId [] (SetComputeUnitPrice microLamports)

-- | Creates instruction to "Set the transaction-wide loaded account data size limit in bytes".
-- Compute Budget instructions take no accounts.
setLoadedAccountsDataSizeLimit :: Word32 -> Instruction
setLoadedAccountsDataSizeLimit bytes = mkInstruction computeBudgetProgramId [] (SetLoadedAccountsDataSizeLimit bytes)
