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
    put (0 :: Word8)
    put lamports
    put space
    put owner
  put (Assign owner) =
    do
      put (1 :: Word8)
      put owner
  put (Transfer lamports) =
    do
      put (2 :: Word8)
      put lamports

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
            }
        ],
      iData = S.toStrict $ encode (Transfer (fromIntegral amount))
    }
