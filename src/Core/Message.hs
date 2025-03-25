{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Message
  ( Instruction (..),
    AccountMeta (..),
    CompileException,
    updateMessageWithInstructions,
    compileMessage,
    newMessage,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (liftEither)
import Control.Monad.Reader
import Core.Block (BlockHash)
import Core.Compact
import Core.Crypto (SolanaPublicKey)
import Data.Binary
import Data.Binary.Put (putByteString)
import Data.ByteString qualified as S
import Data.Either.Combinators (maybeToRight)
import Data.List (elemIndex)
import GHC.Generics (Generic)

------------------------------------------------------------------------------------------------

-- ** Message (Unsigned transaction)

------------------------------------------------------------------------------------------------

-- | The structure of a transaction message.
data Message = Message
  { -- | Specifies the number of signer and read-only account.
    mHeader :: MessageHeader,
    -- |  All the account keys used by this transaction (used by all the instructions on the transaction).
    mAccountKeys :: [SolanaPublicKey],
    -- | The id of a recent ledger entry. Acts as a timestamp for the transaction.
    mRecentBlockhash :: BlockHash,
    {-  An array of instructions to be executed.
        Programs that will be executed in sequence and committed in one atomic transaction if all succeed.
    -}
    mInstructions :: [Instruction]
  }
  deriving (Show, Eq, Generic)

newMessage :: (MonadReader BlockHash m, MonadError CompileException m) => [Instruction] -> m S.ByteString
newMessage is = do
  bh <- ask
  m <- liftEither $ compileMessage (mkNewMessage bh is)
  return $ S.toStrict . Data.Binary.encode $ m

mkNewMessage :: BlockHash -> [Instruction] -> Message
mkNewMessage bh is = updateMessageWithInstructions (Message mempty mempty bh mempty) is

updateMessageWithInstructions :: Message -> [Instruction] -> Message
updateMessageWithInstructions = foldl' updateMessageWithInstruction

updateMessageWithInstruction :: Message -> Instruction -> Message
updateMessageWithInstruction (Message header accountKeys bh instrs) newInstr =
  let (newHeader, newAccountKeys) =
        foldl' updateHeaderAndKeys (header, accountKeys) (iAccounts newInstr)
      newInstrucionsList = instrs <> [newInstr]
   in (Message newHeader newAccountKeys bh newInstrucionsList)

updateHeaderAndKeys :: (MessageHeader, [SolanaPublicKey]) -> AccountMeta -> (MessageHeader, [SolanaPublicKey])
updateHeaderAndKeys (currentHeader, currentKeys) (AccountMeta newKey isSigner isWritable) =
  let (rws, ros, rwus, rous) = splitAccountsByPurpose currentHeader currentKeys
      alreadySignable = newKey `elem` (rws <> ros)
      alreadyWritable = newKey `elem` (rwus <> rous)
      (rws', ros', rwus', rous') =
        case (isSigner || alreadySignable, isWritable || alreadyWritable) of
          (True, True) ->
            ( (addIfNotExists newKey rws),
              (removeAll newKey ros),
              (removeAll newKey rwus),
              (removeAll newKey rous)
            )
          (True, False) ->
            ( rws,
              (addIfNotExists newKey ros),
              rwus,
              (removeAll newKey rous)
            )
          (False, True) ->
            ( rws,
              ros,
              (addIfNotExists newKey rwus),
              (removeAll newKey rous)
            )
          (False, False) ->
            ( rws,
              ros,
              rwus,
              (addIfNotExists newKey rous)
            )
      updatedMessage = mkMessageHeaderFromSplittedAccounts (rws', ros', rwus', rous')
      updatedKeys = (rws' <> ros' <> rwus' <> rous')
   in (updatedMessage, updatedKeys)
  where
    addIfNotExists :: (Eq a) => a -> [a] -> [a]
    addIfNotExists x xs =
      if x `elem` xs
        then xs
        else xs ++ [x]

    removeAll :: (Eq a) => a -> [a] -> [a]
    removeAll x = filter (/= x)

splitAccountsByPurpose ::
  MessageHeader ->
  [SolanaPublicKey] ->
  ([SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey])
splitAccountsByPurpose
  (MessageHeader x y z)
  keys =
    let numRequiredSignatures = fromIntegral x
        numReadonlySigned = fromIntegral y
        numReadonlyUnsigned = fromIntegral z

        -- Split into signed and unsigned keys:
        (signed, unsigned) = splitAt numRequiredSignatures keys
        -- For signed keys: first part are read and write, last part are read-only.
        (readAndWriteSigned, readOnlySigned) = splitAt (numRequiredSignatures - numReadonlySigned) signed
        -- For unsigned keys:
        unsignedCount = length unsigned
        (readAndWriteUnsigned, readOnlyUnsigned) = splitAt (unsignedCount - numReadonlyUnsigned) unsigned
     in (readAndWriteSigned, readOnlySigned, readAndWriteUnsigned, readOnlyUnsigned)

mkMessageHeaderFromSplittedAccounts ::
  ([SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey], [SolanaPublicKey]) ->
  MessageHeader
mkMessageHeaderFromSplittedAccounts (readAndWriteSigned, readOnlySigned, _readAndWriteUnsigned, readOnlyUnsigned) =
  let numRequiredSignatures = fromIntegral $ length readAndWriteSigned + length readOnlySigned
      numReadonlySigned = fromIntegral $ length readOnlySigned
      numReadonlyUnsigned = fromIntegral $ length readOnlyUnsigned
   in MessageHeader numRequiredSignatures numReadonlySigned numReadonlyUnsigned

------------------------------------------------------------------------------------------------

-- *** MessageHeader

------------------------------------------------------------------------------------------------
{- The structure of the Transaction MessageHeader.
The message header uses three bytes to define account privileges
-}
data MessageHeader = MessageHeader
  { {-
    The number of signatures required for this message to be considered
    valid. The signers of those signatures must match the first
    `numRequiredSignatures` of Message's accountKeys.
    -}
    numRequiredSignatures :: Word8,
    {-
    The last `numRequiredSignatures` of the signed keys are read-only accounts.
    -}
    numReadonlySigned :: Word8,
    {-
    The last `numReadonlyUnsigned` of the unsigned keys are read-only accounts.
    -}
    numReadonlyUnsigned :: Word8
  }
  deriving (Show, Eq, Generic)

instance Binary MessageHeader where
  put :: MessageHeader -> Put
  put MessageHeader {..} = do
    put numRequiredSignatures
    put numReadonlySigned
    put numReadonlyUnsigned

  get :: Get MessageHeader
  get = MessageHeader <$> get <*> get <*> get

instance Semigroup MessageHeader where
  (<>) :: MessageHeader -> MessageHeader -> MessageHeader
  (<>) (MessageHeader rqs ros rou) (MessageHeader rqs' ros' rou') = MessageHeader (rqs + rqs') (ros + ros') (rou + rou')

instance Monoid MessageHeader where
  mempty :: MessageHeader
  mempty = MessageHeader 0 0 0

------------------------------------------------------------------------------------------------

-- *** Instruction

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
    iData :: S.ByteString
  }
  deriving (Show, Eq, Generic)

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

-- ** Compiled Message

------------------------------------------------------------------------------------------------

-- | The structure of a Compiled Message.
data CompiledMessage = CompiledMessage
  { -- | Specifies the number of signer and read-only account.
    cmHeader :: MessageHeader,
    -- |  Compact array of account keys used by this transaction (used by all the instructions on the transaction).
    cmAccountKeys :: CompactArray SolanaPublicKey,
    -- | The id of a recent ledger entry. Acts as a timestamp for the transaction.
    cmRecentBlockhash :: BlockHash,
    {-  Compact array of compiled instructions to be executed.
    -}
    cmInstructions :: CompactArray CompiledInstruction
  }
  deriving (Show, Eq, Generic)

instance Binary CompiledMessage where
  put :: CompiledMessage -> Put
  put CompiledMessage {..} = do
    put cmHeader
    put cmAccountKeys
    put cmRecentBlockhash
    put cmInstructions
  get :: Get CompiledMessage
  get = CompiledMessage <$> get <*> get <*> get <*> get

------------------------------------------------------------------------------------------------

-- *** Compile Message

------------------------------------------------------------------------------------------------
compileMessage :: Message -> Either CompileException CompiledMessage
compileMessage Message {..} = do
  compiledInstrctions <- sequence $ compileInstruction mAccountKeys <$> mInstructions
  return $
    CompiledMessage
      { cmHeader = mHeader,
        cmAccountKeys = mkCompact mAccountKeys,
        cmRecentBlockhash = mRecentBlockhash,
        cmInstructions = mkCompact compiledInstrctions
      }

compileInstruction :: [SolanaPublicKey] -> Instruction -> Either CompileException CompiledInstruction
compileInstruction keys (Instruction {..}) = do
  programIdIndex <- keyToIndex iProgramId keys
  accIndices <- sequence $ ((`keyToIndex` keys) . accountPubKey) <$> iAccounts
  return $
    CompiledInstruction
      { ciProgramIdIndex = (fromIntegral programIdIndex),
        ciAccounts = (mkCompact (fromIntegral <$> accIndices)),
        ciData = mkCompact . S.unpack $ iData
      }

keyToIndex :: SolanaPublicKey -> [SolanaPublicKey] -> Either CompileException Int
keyToIndex k keys = maybeToRight (MissingIndex $ show k) $ k `elemIndex` keys

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

instance Binary CompiledInstruction where
  put :: CompiledInstruction -> Put
  put CompiledInstruction {..} = do
    put ciProgramIdIndex
    put ciAccounts
    put ciData
  get :: Get CompiledInstruction
  get = CompiledInstruction <$> get <*> get <*> get

------------------------------------------------------------------------------------------------

-- *** CompileException

------------------------------------------------------------------------------------------------
data CompileException = MissingIndex String
  deriving (Show)
