{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Solana.Core.Instruction
  ( Instruction,
    mkInstruction,
    iProgramId,
    iAccounts,
    iData,
    AccountMeta (..),
    InstructionData (..),
    compileInstruction,
    CompiledInstruction,
    CompileException,
  )
where

import Control.Exception
import Data.Aeson.Types
import Data.Binary
import Data.Binary qualified as Binary
import Data.ByteString qualified as S
import Data.Either.Combinators
import Data.List (elemIndex)
import GHC.Generics (Generic)
import Network.Solana.Core.Compact
import Network.Solana.Core.Crypto (SolanaPublicKey, fromBase58String, toBase58String, toBase64String)

------------------------------------------------------------------------------------------------

-- * Instruction

------------------------------------------------------------------------------------------------

-- | A single program invocation: which program to call, the accounts it
-- may read or write, and the input bytes to pass it.
data Instruction = Instruction
  { -- | Address of the program that executes this instruction.
    iProgramId :: SolanaPublicKey,
    -- |  List of metadata describing accounts that should be passed to the program.
    iAccounts :: [AccountMeta],
    -- | Bytes selecting which instruction of the program to invoke, plus
    -- any arguments it needs.
    iData :: InstructionData
  }
  deriving (Show, Eq, Generic)

-- | Build an 'Instruction' from a program id, account metas, and
-- 'Binary'-encodable instruction data.
mkInstruction :: (Binary.Binary a) => SolanaPublicKey -> [AccountMeta] -> a -> Instruction
mkInstruction programid accmetas instrData =
  Instruction
    { iProgramId = programid,
      iAccounts = accmetas,
      iData = InstructionData $ S.toStrict (Binary.encode instrData)
    }

------------------------------------------------------------------------------------------------

-- ** Account Meta

------------------------------------------------------------------------------------------------

-- | Each account required by an instruction must be provided as an AccountMeta that contains:
data AccountMeta = AccountMeta
  { -- | Account's address
    accountPubKey :: SolanaPublicKey,
    {-  Whether the account must sign the transaction.
        True if an 'Instruction' requires a 'Transaction' signature matching 'SolanaPublicKey'.
    -}
    isSigner :: Bool,
    {-  Whether the instruction will modify the account's data.
        True if the account data or metadata may be mutated during program execution.
    -}
    isWritable :: Bool
  }
  deriving (Show, Eq, Generic)

------------------------------------------------------------------------------------------------

-- ** Instruction Data

------------------------------------------------------------------------------------------------

-- | Raw instruction input bytes. 'Show' renders them in Base64.
newtype InstructionData = InstructionData {instrData :: S.ByteString}
  deriving (Eq, Generic)

instance Show InstructionData where
  show :: InstructionData -> String
  show (InstructionData bs) = toBase64String bs

------------------------------------------------------------------------------------------------

-- *** Compiled Instruction

------------------------------------------------------------------------------------------------

-- | The structure of a compiled instruction.
data CompiledInstruction = CompiledInstruction
  { {-
    Index that points to the program's address in the account addresses array.
    This specifies the program that will process the instruction.
    -}
    ciProgramIdIndex :: Word8,
    -- |  Compact array of indexes that point to the account addresses required for this instruction.
    ciAccounts :: CompactArray Word8,
    {-  Compact byte array specifying the instruction on the program to invoke
        and any function arguments required by the instruction.
    -}
    ciData :: CompactArray Word8
  }
  deriving (Show, Eq, Generic)

instance ToJSON CompiledInstruction where
  toJSON :: CompiledInstruction -> Value
  toJSON (CompiledInstruction programIdIndex accounts iData) =
    object
      [ "programIdIndex" .= programIdIndex,
        "accounts" .= unCompact accounts,
        "data" .= (toBase58String . S.pack . unCompact $ iData)
      ]

instance FromJSON CompiledInstruction where
  parseJSON :: Value -> Parser CompiledInstruction
  parseJSON = withObject "CompiledInstruction" $ \v -> do
    programIdIndex <- v .: "programIdIndex"
    accounts <- v .: "accounts"
    dataStr <- v .: "data"
    bytes <- case fromBase58String dataStr of
      Nothing -> fail "CompiledInstruction: data is not valid base58"
      Just bs -> pure bs
    pure
      CompiledInstruction
        { ciProgramIdIndex = programIdIndex,
          ciAccounts = mkCompact accounts,
          ciData = mkCompact (S.unpack bytes)
        }

instance Binary CompiledInstruction where
  put :: CompiledInstruction -> Put
  put CompiledInstruction {..} = do
    put ciProgramIdIndex
    put ciAccounts
    put ciData
  get :: Get CompiledInstruction
  get = CompiledInstruction <$> get <*> get <*> get

-- | Replace each pubkey referenced by an instruction with its index into
-- the given account-key table. 'Left' when a key is not present in the
-- table.
compileInstruction :: [SolanaPublicKey] -> Instruction -> Either CompileException CompiledInstruction
compileInstruction keys instruction = do
  programIdIndex <- keyToIndex (iProgramId instruction) keys
  accIndices <- mapM ((`keyToIndex` keys) . accountPubKey) (iAccounts instruction)
  return $
    CompiledInstruction
      { ciProgramIdIndex = fromIntegral programIdIndex,
        ciAccounts = mkCompact (fromIntegral <$> accIndices),
        ciData = mkCompact . S.unpack $ instrData (iData instruction)
      }

keyToIndex :: SolanaPublicKey -> [SolanaPublicKey] -> Either CompileException Int
keyToIndex k keys = maybeToRight (MissingIndex $ show k) $ k `elemIndex` keys

------------------------------------------------------------------------------------------------

-- *** CompileException

------------------------------------------------------------------------------------------------
-- | Compilation failure: an instruction references an account key that is
-- missing from the message's account table.
newtype CompileException = MissingIndex String
  deriving (Show)

instance Exception CompileException