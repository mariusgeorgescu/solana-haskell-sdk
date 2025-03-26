{-# LANGUAGE OverloadedStrings #-}

module NativePrograms.SystemProgram where

import Core.Account (Lamport)
import Core.Crypto
import Core.Message
import Data.Binary
import Data.Binary.Put
import Data.ByteString qualified as S
import Data.ByteString.Lazy qualified as L
import GHC.Generics

systemProgramId :: SolanaPublicKey
systemProgramId = unsafeSolanaPublicKey ("11111111111111111111111111111111" :: S.ByteString)

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
    put (getSolanaPublicKeyRawByteString owner)
  put (Assign owner) =
    do
      putWord32le (1 :: Word32)
      put (getSolanaPublicKeyRawByteString owner)
  put (Transfer lamports) =
    do
      putWord32le (2 :: Word32)
      putWord64le lamports

transfer :: SolanaPublicKey -> SolanaPublicKey -> Int -> Instruction
transfer fundingAccount recipientAccount amount =
  Instruction
    { iProgramId = systemProgramId,
      iAccounts =
        [ AccountMeta
            { accountPubKey = fundingAccount,
              isSigner = True,
              isWritable = True
            },
          AccountMeta
            { accountPubKey = recipientAccount,
              isSigner = False,
              isWritable = True
            },
          AccountMeta ----- S-ar putea sa nu fie nevoie
            { accountPubKey = systemProgramId,
              isSigner = False,
              isWritable = False
            }
        ],
      iData = InstructionData $ S.toStrict $ encode (Transfer (fromIntegral amount))
    }
