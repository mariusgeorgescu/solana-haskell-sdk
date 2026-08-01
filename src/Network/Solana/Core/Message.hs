{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Network.Solana.Core.Message
-- Description : Building, canonically ordering, compiling and signing transaction messages.
module Network.Solana.Core.Message
  ( newTransactionIntent,
    newTransactionIntentWithPayer,
    newDurableNonceTransactionIntent,
    SignedTransactionIntent,
    Message (..),
    MessageHeader (..),
    CompiledMessage,
    newMessage,
    newMessageToBase64String,
    mkNewMessage,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Binary
import Data.ByteString qualified as S
import Data.Foldable
import Data.List (sortOn)
import GHC.Generics (Generic)
import Network.Solana.Core.Block (BlockHash)
import Network.Solana.Core.Compact
import Network.Solana.Core.Crypto (SolanaPrivateKey, SolanaPublicKey, dsign, getSolanaPublicKeyRaw, toBase64String, toSolanaPublicKey)
import Network.Solana.Core.Instruction
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram


------------------------------------------------------------------------------------------------

-- * SignedTransactionIntent

------------------------------------------------------------------------------------------------

-- | A transaction awaiting a recent block hash: applying one yields the
-- Base64-encoded signed transaction, or a 'CompileException'.
type SignedTransactionIntent = (BlockHash -> Either CompileException String)


-- | Builds and signs a transaction from a list of signing keys and
-- instructions. The fee payer is the first writable signer encountered
-- across the instructions' account metas, and account keys are canonically
-- ordered (fee payer first, then each privilege section sorted by pubkey
-- bytes; see @canonicalizeAccountOrder@). The private keys passed here must
-- be ordered to match the resulting account order (fee payer first),
-- otherwise the produced signatures will not correspond to the right
-- account keys. If you would rather name the fee payer explicitly and not
-- worry about signer order, consider 'newTransactionIntentWithPayer',
-- which orders signatures automatically.
newTransactionIntent :: [SolanaPrivateKey] -> [Instruction] -> SignedTransactionIntent
newTransactionIntent signers instructions blockhash = do
  msg <- newMessage blockhash instructions -- make the binary message
  let signatures = S.toStrict . Data.Binary.encode $ mkCompact $ flip dsign msg <$> signers -- sign the binary message
  return $ toBase64String $ S.append signatures msg -- return signed transaction

-- | Builds and signs a transaction with an explicitly named fee payer.
-- Unlike 'newTransactionIntent', the fee payer is always account 0
-- (sponsored fees are supported: the payer need not appear in any
-- instruction at all), and the given private keys may be listed in any
-- order — they are matched against the compiled message's required
-- signers and the resulting signatures are placed in message order
-- automatically. 'Left' if a required signer has no corresponding private
-- key, or if a given private key does not correspond to any required
-- signer.
newTransactionIntentWithPayer :: SolanaPublicKey -> [SolanaPrivateKey] -> [Instruction] -> BlockHash -> Either CompileException String
newTransactionIntentWithPayer payer signingKeys instructions blockhash = do
  let msg = mkNewMessageWithPayer payer blockhash instructions
  msgBytes <- compileMessageToBinary msg
  let requiredSigners = take (fromIntegral (numRequiredSignatures (mHeader msg))) (mAccountKeys msg)
      keyedByPubkey = [(toSolanaPublicKey k, k) | k <- signingKeys]
  orderedKeys <- mapM (findSigningKey keyedByPubkey) requiredSigners
  mapM_ (checkKeyIsRequired requiredSigners . fst) keyedByPubkey
  let signatures = S.toStrict . Data.Binary.encode $ mkCompact $ flip dsign msgBytes <$> orderedKeys
  return $ toBase64String $ S.append signatures msgBytes
  where
    findSigningKey keyedByPubkey pk =
      case lookup pk keyedByPubkey of
        Just k -> Right k
        Nothing -> Left (MissingIndex ("missing signer for " <> show pk))
    checkKeyIsRequired requiredSigners pk =
      if pk `elem` requiredSigners
        then Right ()
        else Left (MissingIndex ("unused signing key " <> show pk))

-- | Builds and signs a durable-nonce transaction: prepends
-- @SystemProgram.advanceNonceAccount nonceAccount nonceAuthority@ ahead of
-- the given instructions and delegates to 'newTransactionIntent'. The
-- durable nonce — read from the nonce account's on-chain 'NonceState' via
-- @nsDurableNonce@ — is passed in place of a recent block hash, since a
-- durable-nonce transaction never expires and does not depend on a recent
-- ledger entry. The nonce authority must be among the given signers, or the
-- resulting transaction will be rejected on submission.
newDurableNonceTransactionIntent :: [SolanaPrivateKey] -> SolanaPublicKey -> SolanaPublicKey -> [Instruction] -> BlockHash -> Either CompileException String
newDurableNonceTransactionIntent signers nonceAccount nonceAuthority instructions =
  newTransactionIntent signers (SystemProgram.advanceNonceAccount nonceAccount nonceAuthority : instructions)

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

-- | Compile instructions into serialized legacy-message bytes using the
-- given recent block hash. Account keys are collected and canonically
-- ordered as described at 'newTransactionIntent'.
newMessage :: BlockHash -> [Instruction] -> Either CompileException S.ByteString
newMessage bh is = compileMessageToBinary (mkNewMessage bh is)

-- | Like 'newMessage', but returns the message bytes Base64-encoded.
newMessageToBase64String :: BlockHash -> [Instruction] -> Either CompileException String
newMessageToBase64String = fmap (fmap toBase64String) . newMessage

compileMessageToBinary :: Message -> Either CompileException S.ByteString
compileMessageToBinary = fmap (S.toStrict . Data.Binary.encode) . compileMessage

-- | Assemble the uncompiled 'Message': fold the instructions' account
-- metas into a deduplicated, privilege-merged key list, then put it in
-- canonical order.
mkNewMessage :: BlockHash -> [Instruction] -> Message
mkNewMessage bh =
  canonicalizeAccountOrder . updateMessageWithInstructions (Message mempty mempty bh mempty)

-- | Like 'mkNewMessage', but with an explicitly named fee payer. The payer
-- is seeded as a writable signer before the instructions' account metas are
-- folded in, so it ends up in the message (and pinned first by
-- 'canonicalizeAccountOrder') even if no instruction references it —
-- enabling sponsored-fee transactions.
mkNewMessageWithPayer :: SolanaPublicKey -> BlockHash -> [Instruction] -> Message
mkNewMessageWithPayer payer bh =
  canonicalizeAccountOrder . updateMessageWithInstructions seeded
  where
    payerMeta = AccountMeta {accountPubKey = payer, isSigner = True, isWritable = True}
    (seededHeader, seededKeys) = updateHeaderAndKeys (mempty, mempty) payerMeta
    seeded = Message seededHeader seededKeys bh mempty

-- | Sort the account keys within each privilege section by raw pubkey bytes,
-- keeping the fee payer (first writable signer) pinned in first position.
-- Matches the Rust SDK's CompiledKeys (BTreeMap) ordering, which is required
-- for byte-identical messages once several program ids share a section.
canonicalizeAccountOrder :: Message -> Message
canonicalizeAccountOrder (Message header keys bh instrs) =
  let (rws, ros, rwus, rous) = splitAccountsByPurpose header keys
      sortKeys = sortOn getSolanaPublicKeyRaw
      rws' = case rws of
        [] -> []
        (feePayer : rest) -> feePayer : sortKeys rest
      keys' = rws' <> sortKeys ros <> sortKeys rwus <> sortKeys rous
   in Message header keys' bh instrs

updateMessageWithInstructions :: Message -> [Instruction] -> Message
updateMessageWithInstructions = foldl' updateMessageWithInstruction

updateMessageWithInstruction :: Message -> Instruction -> Message
updateMessageWithInstruction (Message header accountKeys bh instrs) newInstr =
  let programMeta =
        AccountMeta
          { accountPubKey = iProgramId newInstr,
            isSigner = False,
            isWritable = False
          }
      (newHeader, newAccountKeys) =
        foldl' updateHeaderAndKeys (header, accountKeys) (iAccounts newInstr <> [programMeta])
      newInstrucionsList = instrs <> [newInstr]
   in Message newHeader newAccountKeys bh newInstrucionsList

updateHeaderAndKeys :: (MessageHeader, [SolanaPublicKey]) -> AccountMeta -> (MessageHeader, [SolanaPublicKey])
updateHeaderAndKeys (currentHeader, currentKeys) (AccountMeta newKey isSigner isWritable) =
  let (rws, ros, rwus, rous) = splitAccountsByPurpose currentHeader currentKeys
      alreadySignable = newKey `elem` rws <> ros
      alreadyWritable = newKey `elem` rws <> rwus
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
-- | Three bytes at the front of every message declaring account privileges.
data MessageHeader = MessageHeader
  { -- | Number of signatures required for the message to be valid; the
    -- first @numRequiredSignatures@ account keys of the message must sign,
    -- in order.
    numRequiredSignatures :: Word8,
    -- | Of the signed keys, the last @numReadonlySignedAccounts@ are
    -- read-only.
    numReadonlySignedAccounts :: Word8,
    -- | Of the unsigned keys, the last @numReadonlyUnsignedAccounts@ are
    -- read-only.
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
