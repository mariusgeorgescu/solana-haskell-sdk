module Core.TransactionBuilder where

import Core.Block (BlockHash)
import Core.Compact (mkCompact)
import Core.Crypto
import Data.Binary
import Data.ByteString qualified as S
import GHC.Generics

------------------------------------------------------------------------------------------------

-- * Transaction Builder Class

------------------------------------------------------------------------------------------------

type TxId = S.ByteString

class SolanaTxBuilder m where
  getRecentBlockHash :: m BlockHash
  submitTx :: Transaction -> m TxId

------------------------------------------------------------------------------------------------

-- * Transaction

------------------------------------------------------------------------------------------------

-- | The structure of a Solana transaction.
data Transaction = Transaction
  { -- | An array of signatures included on the transaction.
    txSignatures :: [SolanaSignature],
    -- | Binary representation of the Message containing instructions to be processed atomically (and impacted accounts).
    txMessage :: S.ByteString
  }
  deriving (Show, Eq, Generic)

-- instance Binary Transaction where
--   put :: Transaction -> Put
--   put (Transaction sigs msg) = do
--     put (mkCompact sigs)
--     put msg

-- addInstruction :: Transaction -> Instruction -> Transaction
-- addInstruction (Transaction sigs msg) newInstr =
--   (Transaction sigs (updateMessageWithInstruction msg newInstr))

-- data SolanaTxBuildingException = SolanaTxBuildingException

-- type SolanaTxBuilder = StateT Transaction (Either SolanaTxBuildingException)
