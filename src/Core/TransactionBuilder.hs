module Core.TransactionBuilder where

-- import Core.Block (BlockHash, unsafeBlockHash)
-- import Core.Compact (mkCompact)
-- import Core.Crypto
-- import Core.Message
-- import Data.Aeson (ToJSON (..))
-- import Data.Base64.Types
-- import Data.Binary
-- import Data.ByteString qualified as S
-- import GHC.Generics
-- import NativePrograms.SystemProgram
-- import Data.ByteString.Base64 (encodeBase64')

-- ------------------------------------------------------------------------------------------------

-- -- * Transaction Builder Class

-- ------------------------------------------------------------------------------------------------

-- type TxId = S.ByteString

-- -- class SolanaTxBuilder m where
-- --   getRecentBlockHash :: m BlockHash
-- --   submitTx :: Transaction -> m TxId

-- -- instance Binary Transaction where
-- --   put :: Transaction -> Put
-- --   put (Transaction sigs msg) = do
-- --     put (mkCompact sigs)
-- --     put msg

-- -- addInstruction :: Transaction -> Instruction -> Transaction
-- -- addInstruction (Transaction sigs msg) newInstr =
-- --   (Transaction sigs (updateMessageWithInstruction msg newInstr))

-- -- data SolanaTxBuildingException = SolanaTxBuildingException

-- -- type SolanaTxBuilder = StateT Transaction (Either SolanaTxBuildingException)

-- blockhash :: BlockHash
-- blockhash = unsafeBlockHash "ksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L"

-- user1 :: SolanaPublicKey
-- user1 = unsafeSolanaPublicKey "3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs"

-- user2 :: SolanaPublicKey
-- user2 = unsafeSolanaPublicKey "Bksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L"

-- runNewMessage :: BlockHash -> [Instruction] -> S.ByteString
-- runNewMessage bh instrs = case newMessage bh instrs of
--   Left err -> error (show err)
--   Right msg -> msg

-- instr = [transfer user1 user2 1000000000]

-- uncompiledMsg :: Message
-- uncompiledMsg = mkNewMessage blockhash instr

-- compiledMsg :: CompiledMessage
-- compiledMsg = case compileMessage uncompiledMsg of
--   Left err -> error (show err)
--   Right msg -> msg

-- newMsg :: S.ByteString
-- newMsg = runNewMessage blockhash instr --

-- --- >>>  uncompiledMsg
-- -- Message {mHeader = MessageHeader {numRequiredSignatures = 1, numReadonlySigned = 0, numReadonlyUnsigned = 1}, mAccountKeys = ["3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs","Bksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L","11111111111111111111111111111111"], mRecentBlockhash = "ksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L", mInstructions = [Instruction {iProgramId = "11111111111111111111111111111111", iAccounts = [AccountMeta {accountPubKey = "3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs", isSigner = True, isWritable = True},AccountMeta {accountPubKey = "Bksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L", isSigner = False, isWritable = True},AccountMeta {accountPubKey = "11111111111111111111111111111111", isSigner = False, isWritable = False}], iData = "AgAAAADKmjsAAAAA"}]}
-- --- >>>  compiledMsg
-- -- CompiledMessage {cmHeader = MessageHeader {numRequiredSignatures = 1, numReadonlySigned = 0, numReadonlyUnsigned = 1}, cmAccountKeys = CompactArray 3 ["3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs","Bksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L","11111111111111111111111111111111"], cmRecentBlockhash = "ksn8qDDcFdgT1LAD8GoDM8THC8J2AnvCKkvKEmA9Q7L", cmInstructions = CompactArray 1 [CompiledInstruction {ciProgramIdIndex = 2, ciAccounts = CompactArray 3 [0,1,2], ciData = CompactArray 12 [2,0,0,0,0,202,154,59,0,0,0,0]}]}

-- xy = encodeBase64'

-- --- >>>  encodeBase64' . S.toStrict . encode $ compiledMsg
-- -- "AQABAyfgZ//LagyLlniL71M0M81bKx26tMzIYFAvhppWUoIQn9JCVsDCwdrk/xbn6uKc/byb5E5+K4CvHoU7gH8dfzsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAs9Y86y3JrlY4X/BoOsd79OhyFfvknN4Gx9q4B/HX87AQIDAAECDAIAAAAAypo7AAAAAA=="

-- --- >>>  encodeBase64' $ newMsg
-- -- "AQABAyfgZ//LagyLlniL71M0M81bKx26tMzIYFAvhppWUoIQn9JCVsDCwdrk/xbn6uKc/byb5E5+K4CvHoU7gH8dfzsAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAs9Y86y3JrlY4X/BoOsd79OhyFfvknN4Gx9q4B/HX87AQIDAAECDAIAAAAAypo7AAAAAA=="

-- normallist = encode ([1, 2, 4, 5, 6, 7, 8] :: [Word8])

-- compactlist = encode (mkCompact ([1, 2, 4, 5, 6, 7, 8] :: [Word8]))

-- --- >>>  encode $ normallist
-- --- >>>  encode $ compactlist
-- -- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SI\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\SOH\STX\EOT\ENQ\ACK\a\b"
-- -- "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\a\SOH\STX\EOT\ENQ\ACK\a\b"

-- compiled = "AQABAz1/AORqAMJK+uQvTafb9umEAGizV0aBNfFKv+xozAJxyqUklSlTmRgk+Qci7TBxmpJ3Agda2RdolwXr3uX0YjkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAJ/SQlbAwsHa5P8W5+rinP28m+ROfiuArx6FO4B/HX87AQICAAEMAgAAAADKmjsAAAAA"

-- x = toJSON user1

-- --- >>> x
-- -- String "\"3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs\""

-- -- Variable not in scope: toJSON :: SolanaPublicKey -> t_avnW[sk:1]
