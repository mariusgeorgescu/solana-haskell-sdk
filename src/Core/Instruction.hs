module Core.Instruction
  ( Instruction,
    mkInstruction,
    iProgramId,
    iAccounts,
    iData,
    AccountMeta (..),
    InstructionData (..),
  )
where

import Core.Crypto (SolanaPublicKey, toBase64String)
import Data.Binary qualified as Binary
import Data.ByteString qualified as S
import Data.ByteString.Base64 (encodeBase64')
import GHC.Generics (Generic)

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
