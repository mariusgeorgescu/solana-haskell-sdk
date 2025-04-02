module Network.Solana.NativePrograms.SystemProgram where

import Data.Binary
import Data.Binary.Put
import GHC.Generics
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction

systemProgramId :: SolanaPublicKey
systemProgramId = unsafeSolanaPublicKey "11111111111111111111111111111111"

data SystemInstruction
  = CreateAccount
      { lamports :: Word64,
        space :: Word64,
        owner :: SolanaPublicKey
      }
  | Assign {owner :: SolanaPublicKey}
  | Transfer {lamports :: Word64}
  deriving (Eq, Show, Generic)

instance Binary SystemInstruction where
  put :: SystemInstruction -> Put
  put (CreateAccount lamports space owner) = do
    putWord32le (0 :: Word32)
    putWord64le lamports
    putWord64le space
    put (getSolanaPublicKeyRaw owner)
  put (Assign owner) =
    do
      putWord32le (1 :: Word32)
      put (getSolanaPublicKeyRaw owner)
  put (Transfer lamports) =
    do
      putWord32le (2 :: Word32)
      putWord64le lamports

transfer :: SolanaPublicKey -> SolanaPublicKey -> Int -> Instruction
transfer fundingAccount recipientAccount amount =
  mkInstruction
    systemProgramId
    [ AccountMeta
        { accountPubKey = fundingAccount,
          isSigner = True,
          isWritable = True
        },
      AccountMeta
        { accountPubKey = recipientAccount,
          isSigner = False,
          isWritable = True
        }
    ]
    (Transfer (fromIntegral amount))
