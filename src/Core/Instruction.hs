module Core.Instruction where

import Core.Crypto (SolanaPublicKey)
import Data.ByteString qualified as S
import GHC.Generics (Generic)

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

data Instruction = Instruction
  { {-  Pubkey of the program that executes this instruction.
        The program being invoked to execute the instruction.
    -}
    instructionProgramId :: SolanaPublicKey,
    -- |  List of metadata describing accounts that should be passed to the program.
    instructionAccounts :: [AccountMeta],
    {-  Byte array specifying the instruction on the program to invoke
        and any function arguments required by the instruction.
    -}
    instructionData :: S.ByteString
  }
  deriving (Show, Eq, Generic)