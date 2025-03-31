{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Core.Message
  ( newTransactionIntent,
    Message,
    CompiledMessage,
    newMessage,
    newMessageToBase64String,
  )
where

import Control.Exception
import Core.Block (BlockHash)
import Core.Compact
import Core.Crypto (SolanaPrivateKey, SolanaPublicKey, dsign, toBase64String)
import Core.Instruction
import Data.Aeson
import Data.Aeson.Types (Parser)
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
  let signatures = S.toStrict . Data.Binary.encode $ mkCompact $ flip dsign msg <$> signers -- sign the binary message
  return $ toBase64String $ S.append signatures msg -- return signed transaction

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
newMessage bh is = compileMessageToBinary (mkNewMessage bh is)

newMessageToBase64String :: BlockHash -> [Instruction] -> Either CompileException String
newMessageToBase64String = fmap (fmap toBase64String) . newMessage

compileMessageToBinary :: Message -> Either CompileException S.ByteString
compileMessageToBinary = fmap (S.toStrict . Data.Binary.encode) . compileMessage

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
    numReadonlySignedAccounts :: Word8,
    {-
    The last `numReadonlyUnsigned` of the unsigned keys are read-only accounts.
    -}
    numReadonlyUnsignedAccounts :: Word8
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Binary MessageHeader where
  put :: MessageHeader -> Put
  put MessageHeader {..} = do
    put numRequiredSignatures
    put numReadonlySignedAccounts
    put numReadonlyUnsignedAccounts

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

instance ToJSON CompiledMessage where
  toJSON :: CompiledMessage -> Value
  toJSON (CompiledMessage header accountKeys recentBlockhash instructions) =
    object
      [ "header" .= header,
        "accountKeys" .= unCompact accountKeys,
        "recentBlockhash" .= recentBlockhash,
        "instructions" .= unCompact instructions
      ]

instance FromJSON CompiledMessage where
  parseJSON :: Value -> Parser CompiledMessage
  parseJSON = withObject "CompiledMessage" $ \v ->
    CompiledMessage
      <$> v .: "header"
      <*> (mkCompact <$> (v .: "accountKeys"))
      <*> v .: "recentBlockhash"
      <*> (mkCompact <$> (v .: "instructions"))

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
