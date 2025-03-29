{-# LANGUAGE RecordWildCards #-}

module Core.Message
  ( newTransactionIntent,
    Message,
  )
where

import Control.Exception
import Core.Block (BlockHash)
import Core.Compact
import Core.Crypto (SolanaPrivateKey, SolanaPublicKey, dsign)
import Core.Instruction
import Data.Binary
import Data.ByteString qualified as S
import Data.ByteString.Base64 (encodeBase64')
import Data.Either.Combinators (maybeToRight)
import Data.Foldable
import Data.List (elemIndex)
import GHC.Generics (Generic)

------------------------------------------------------------------------------------------------

-- * SignedTransactionIntent

------------------------------------------------------------------------------------------------

type SignedTransactionIntent = (BlockHash -> Either CompileException String)

newTransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> SignedTransactionIntent
newTransactionIntent signers instructions blockhash = do
  msg <- newMessage blockhash instructions -- make the binary message
  let signatures = S.toStrict . encode $ mkCompact $ flip dsign msg <$> signers -- sign the binary message
  return $ tail . init . show $ encodeBase64' $ S.append signatures msg -- return signed transaction

------------------------------------------------------------------------------------------------

-- ** Message

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

newMessage :: BlockHash -> [Instruction] -> Either CompileException S.ByteString
newMessage bh is = do
  compiled <- compileMessage (mkNewMessage bh is)
  return $ S.toStrict . Data.Binary.encode $ compiled

mkNewMessage :: BlockHash -> [Instruction] -> Message
mkNewMessage bh = updateMessageWithInstructions (Message mempty mempty bh mempty)

updateMessageWithInstructions :: Message -> [Instruction] -> Message
updateMessageWithInstructions = foldl' updateMessageWithInstruction

updateMessageWithInstruction :: Message -> Instruction -> Message
updateMessageWithInstruction (Message header accountKeys bh instrs) newInstr =
  let (newHeader, newAccountKeys) =
        foldl' updateHeaderAndKeys (header, accountKeys) (iAccounts newInstr)
      newInstrucionsList = instrs <> [newInstr]
   in Message newHeader newAccountKeys bh newInstrucionsList

updateHeaderAndKeys :: (MessageHeader, [SolanaPublicKey]) -> AccountMeta -> (MessageHeader, [SolanaPublicKey])
updateHeaderAndKeys (currentHeader, currentKeys) (AccountMeta newKey isSigner isWritable) =
  let (rws, ros, rwus, rous) = splitAccountsByPurpose currentHeader currentKeys
      alreadySignable = newKey `elem` rws <> ros
      alreadyWritable = newKey `elem` rwus <> rous
      (rws', ros', rwus', rous') =
        case (isSigner || alreadySignable, isWritable || alreadyWritable) of
          (True, True) ->
            ( addIfNotExists newKey rws,
              removeAll newKey ros,
              removeAll newKey rwus,
              removeAll newKey rous
            )
          (True, False) ->
            ( rws,
              addIfNotExists newKey ros,
              rwus,
              removeAll newKey rous
            )
          (False, True) ->
            ( rws,
              ros,
              addIfNotExists newKey rwus,
              removeAll newKey rous
            )
          (False, False) ->
            ( rws,
              ros,
              rwus,
              addIfNotExists newKey rous
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

------------------------------------------------------------------------------------------------

-- *** Compile Message

------------------------------------------------------------------------------------------------
compileMessage :: Message -> Either CompileException CompiledMessage
compileMessage Message {..} = do
  compiledInstrctions <- mapM (compileInstruction mAccountKeys) mInstructions
  return $
    CompiledMessage
      { cmHeader = mHeader,
        cmAccountKeys = mkCompact mAccountKeys,
        cmRecentBlockhash = mRecentBlockhash,
        cmInstructions = mkCompact compiledInstrctions
      }

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
newtype CompileException = MissingIndex String
  deriving (Show)

instance Exception CompileException