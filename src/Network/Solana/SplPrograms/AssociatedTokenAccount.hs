{-# LANGUAGE OverloadedStrings #-}

-- | Client for the Associated Token Account (ATA) program: derives the
-- canonical token account address for a (wallet, mint) pair and builds
-- create instructions. <https://spl.solana.com/associated-token-account>
module Network.Solana.SplPrograms.AssociatedTokenAccount where

import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Maybe (fromMaybe)
import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw)
import Network.Solana.Core.Instruction
import Network.Solana.Core.Pda (findProgramAddress)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.SplPrograms.Token qualified as Token

-- | Associated Token Account program address.
associatedTokenProgramId :: SolanaPublicKey
associatedTokenProgramId = "ATokenGPvbdGVxr1b2hvZbsiqW5xWH25efTNsLJA8knL"

-- | System program reference (re-exported for account-meta lists).
systemProgramRef :: SolanaPublicKey
systemProgramRef = SystemProgram.systemProgramId

-- | ATA instruction data: a single borsh discriminant byte.
data AtaInstruction = Create | CreateIdempotent
  deriving (Eq, Show)

instance Binary AtaInstruction where
  put :: AtaInstruction -> Put
  put Create = putWord8 0
  put CreateIdempotent = putWord8 1
  get :: Get AtaInstruction
  get = do
    b <- getWord8
    case b of
      0 -> pure Create
      1 -> pure CreateIdempotent
      _ -> fail ("AtaInstruction: unknown discriminant " <> show b)

-- | Derive the associated token account address for a wallet and mint:
-- the PDA of [wallet, token program, mint] under the ATA program.
getAssociatedTokenAddress :: SolanaPublicKey -> SolanaPublicKey -> Maybe SolanaPublicKey
getAssociatedTokenAddress wallet mint =
  fst
    <$> findProgramAddress
      [ getSolanaPublicKeyRaw wallet,
        getSolanaPublicKeyRaw Token.tokenProgramId,
        getSolanaPublicKeyRaw mint
      ]
      associatedTokenProgramId

ataMetas :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> [AccountMeta]
ataMetas funder wallet mint =
  let ata = fromMaybe (error "getAssociatedTokenAddress: derivation failed") (getAssociatedTokenAddress wallet mint)
   in [ AccountMeta {accountPubKey = funder, isSigner = True, isWritable = True},
        AccountMeta {accountPubKey = ata, isSigner = False, isWritable = True},
        AccountMeta {accountPubKey = wallet, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = mint, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = systemProgramRef, isSigner = False, isWritable = False},
        AccountMeta {accountPubKey = Token.tokenProgramId, isSigner = False, isWritable = False}
      ]

-- | Create the associated token account for (wallet, mint), paid by the funder.
-- Fails the transaction if it already exists.
createAssociatedTokenAccount :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
createAssociatedTokenAccount funder wallet mint =
  mkInstruction associatedTokenProgramId (ataMetas funder wallet mint) Create

-- | Like 'createAssociatedTokenAccount' but succeeds (no-op) if the account
-- already exists.
createAssociatedTokenAccountIdempotent :: SolanaPublicKey -> SolanaPublicKey -> SolanaPublicKey -> Instruction
createAssociatedTokenAccountIdempotent funder wallet mint =
  mkInstruction associatedTokenProgramId (ataMetas funder wallet mint) CreateIdempotent
