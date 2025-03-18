{-# LANGUAGE OverloadedStrings #-}

module Transaction.Types where

import Account.Types
import Data.ByteString qualified as B
import Data.Vector
import Data.Word
import GHC.Generics

data CompactArray a = CompactArray Word16 (Vector a)
  deriving (Eq, Show)

data Transaction = Transaction
  { signatures :: CompactArray Signature,
    message :: Message
  }
  deriving (Show, Eq, Generic)

-- |  Signature as a 64-byte ByteString
newtype Signature = Signature B.ByteString deriving (Show, Eq, Generic)

-- Define Message
data Message = Message
  { -- | Specifies the number of signer and read-only account.
    header :: MessageHeader,
    -- |  All the account keys used by this transaction.
    accountKeys :: CompactArray PubKey,
    -- | The id of a recent ledger entry. Acts as a timestamp for the transaction.
    recentBlockhash :: BlockHash,
    -- | An array of instructions to be executed.
    instructions :: CompactArray CompiledInstruction
  }
  deriving (Show, Eq, Generic)

-- Define MessageHeader
data MessageHeader = MessageHeader
  { numRequiredSignatures :: Word8,
    numReadonlySigned :: Word8,
    numReadonlyUnsigned :: Word8
  }
  deriving (Show, Eq, Generic)

-- Define CompiledInstruction (indices to account keys, program ID, and data)
data CompiledInstruction = CompiledInstruction
  { compiledInstructionProgramIdIndex :: Word8,
    compiledInstructionAccounts :: CompactArray Word8,
    compiledInstructionData :: CompactArray Word8
  }
  deriving (Show, Eq, Generic)

newtype BlockHash = BlockHash B.ByteString
  deriving (Show, Eq, Generic)

data AccountMeta = AccountMeta
  { pubkey :: PubKey,
    isSigner :: Bool,
    isWritable :: Bool
  }
  deriving (Show, Eq, Generic)

-------
-------
-------
-------
-------
-------
-- data Instruction = Instruction
--   { instructionProgramId :: PubKey,
--     instructionAccounts :: Vector AccountMeta,
--     instructionData :: B.ByteString
--   }
--   deriving (Show, Eq, Generic)