{-# LANGUAGE DeriveAnyClass #-}

module Core.Transaction where

import Core.Block (BlockHash)
import Core.Crypto (Signature, SolanaPublicKey)
import Core.Instruction (AccountMeta (..), Instruction (..))
import Data.Word (Word8)
import GHC.Generics (Generic)

------------------------------------------------------------------------------------------------

-- * Transaction

------------------------------------------------------------------------------------------------

-- | The structure of a Solana transaction.
data Transaction = Transaction
  { -- | An array of signatures included on the transaction.
    txSignatures :: [Signature],
    -- | Message containing instructions to be processed atomically (and impacted accounts).
    txMessage :: Message
  }
  deriving (Show, Eq, Generic)

instance Semigroup Transaction where
  (<>) :: Transaction -> Transaction -> Transaction
  (<>) (Transaction sigs1 msg1) (Transaction sigs2 msg2) = (Transaction (sigs1 <> sigs2) (msg1 <> msg2))

instance Monoid Transaction where
  mempty :: Transaction
  mempty = Transaction mempty mempty

addIfNotExists :: (Eq a) => a -> [a] -> [a]
addIfNotExists x xs =
  if x `elem` xs
    then xs
    else xs ++ [x]

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll x = filter (/= x)

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
      newMessage = mkMessageHeaderFromSplittedAccounts (rws', ros', rwus', rous')
      newKeys = (rws' <> ros' <> rwus' <> rous')
   in (newMessage, newKeys)

updateMessageWithInstruction :: Message -> Instruction -> Message
updateMessageWithInstruction (Message header accountKeys bh instrs) newInstr =
  let (newHeader, newAccountKeys) =
        foldl' updateHeaderAndKeys (header, accountKeys) (instructionAccounts newInstr)
      newInstrucionsList = instrs <> [newInstr]
   in (Message newHeader newAccountKeys bh newInstrucionsList)

updateMessageWithInstructions :: Message -> [Instruction] -> Message
updateMessageWithInstructions = foldl' updateMessageWithInstruction

addInstruction :: Transaction -> Instruction -> Transaction
addInstruction (Transaction sigs msg) newInstr =
  (Transaction sigs (updateMessageWithInstruction msg newInstr))

mkTransaction :: [Instruction] -> Transaction
mkTransaction = foldl' addInstruction mempty

------------------------------------------------------------------------------------------------

-- ** Message

------------------------------------------------------------------------------------------------

-- | The structure of a transaction message.
data Message = Message
  { -- | Specifies the number of signer and read-only account.
    header :: MessageHeader,
    -- |  All the account keys used by this transaction (used by all the instructions on the transaction).
    accountKeys :: [SolanaPublicKey],
    -- | The id of a recent ledger entry. Acts as a timestamp for the transaction.
    recentBlockhash :: BlockHash,
    {-  An array of instructions to be executed.
        Programs that will be executed in sequence and committed in one atomic transaction if all succeed.
    -}
    instructions :: [Instruction]
  }
  deriving (Show, Eq, Generic)

instance Semigroup Message where
  (<>) :: Message -> Message -> Message
  (<>) m1 m2 = (updateMessageWithInstructions m1 (instructions m2)) {recentBlockhash = (recentBlockhash m2)}

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

instance Monoid Message where
  mempty :: Message
  mempty = Message mempty mempty mempty mempty

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

instance Semigroup MessageHeader where
  (<>) :: MessageHeader -> MessageHeader -> MessageHeader
  (<>) (MessageHeader rqs ros rou) (MessageHeader rqs' ros' rou') = MessageHeader (rqs + rqs') (ros + ros') (rou + rou')

instance Monoid MessageHeader where
  mempty :: MessageHeader
  mempty = MessageHeader 0 0 0
