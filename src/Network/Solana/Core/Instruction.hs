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
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Network.Solana.Core.Compact
import Network.Solana.Core.Crypto (SolanaPublicKey, fromBase58String, toBase58String, toBase64String)

------------------------------------------------------------------------------------------------

-- * Instruction

------------------------------------------------------------------------------------------------

data Instruction = Instruction
  { {-  Pubkey of the program that executes this instruction.
        The program being invoked to execute the instruction.
    -}
    iProgramId :: SolanaPublicKey,
    -- |  List of metadata describing accounts that should be passed to the program.
    iAccounts :: [AccountMeta],
    {-  Byte array specifying the instruction on the program to invoke
        and any function arguments required by the instruction.
    -}
    iData :: InstructionData
  }
  deriving (Show, Eq, Generic)

mkInstruction :: (Binary.Binary a) => SolanaPublicKey -> [AccountMeta] -> a -> Instruction
mkInstruction programid accmetas instrData =
  Instruction
    { iProgramId = programid,
      iAccounts =
        accmetas
          <> [ AccountMeta
                 { accountPubKey = programid,
                   isSigner = False,
                   isWritable = False
                 }
             ],
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

newtype InstructionData = InstructionData {instrData :: S.ByteString}
  deriving (Eq, Generic)

instance Show InstructionData where
  show :: InstructionData -> String
  show (InstructionData bs) = toBase64String bs

------------------------------------------------------------------------------------------------

-- *** Compiled Instruction

------------------------------------------------------------------------------------------------

-- | The structure of a Compiled Instructuin.
data CompiledInstruction = CompiledInstruction
  { {-
    Index that points to the program's address in the account addresses array.
    This specifies the program that will process the instruction.
    -}
    ciProgramIdIndex :: Word8,
    -- |  Compact array of indexes that point to the account addresses required for this instruction..
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
  parseJSON = withObject "CompiledInstruction" $ \v ->
    CompiledInstruction
      <$> v .: "programIdIndex"
      <*> (mkCompact <$> (v .: "accounts"))
      <*> (mkCompact . S.unpack . fromJust . fromBase58String <$> (v .: "data"))

instance Binary CompiledInstruction where
  put :: CompiledInstruction -> Put
  put CompiledInstruction {..} = do
    put ciProgramIdIndex
    put ciAccounts
    put ciData
  get :: Get CompiledInstruction
  get = CompiledInstruction <$> get <*> get <*> get

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
newtype CompileException = MissingIndex String
  deriving (Show)

instance Exception CompileException