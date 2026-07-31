{-# LANGUAGE OverloadedStrings #-}

-- | Program Derived Addresses (PDAs): addresses derived from seeds and a
-- program id that are guaranteed to lie off the ed25519 curve, so no
-- private key can exist for them. Used by programs to sign via CPI and by
-- clients to locate program-owned accounts (e.g. associated token accounts).
module Network.Solana.Core.Pda
  ( PdaError (..),
    createProgramAddress,
    findProgramAddress,
  )
where

import Crypto.ECC.Edwards25519 qualified as Edwards
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Network.Solana.Core.Crypto (SolanaPublicKey, getSolanaPublicKeyRaw, unsafeSolanaPublicKeyRaw)

data PdaError
  = -- | A seed exceeds 32 bytes
    SeedTooLong
  | -- | More than 16 seeds were provided
    TooManySeeds
  | -- | The derived hash landed on the ed25519 curve; use another bump/seed
    InvalidSeeds
  deriving (Eq, Show)

pdaMarker :: BS.ByteString
pdaMarker = "ProgramDerivedAddress"

-- | Derive a program address from seeds and a program id. Fails with
-- 'InvalidSeeds' if the sha256 result is a valid ed25519 curve point.
createProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Either PdaError SolanaPublicKey
createProgramAddress seeds programId
  | length seeds > 16 = Left TooManySeeds
  | any ((> 32) . BS.length) seeds = Left SeedTooLong
  | otherwise =
      let digest :: Digest SHA256
          digest = hash (BS.concat seeds <> getSolanaPublicKeyRaw programId <> pdaMarker)
          bytes = BS.pack (BA.unpack digest)
       in if isOnCurve bytes
            then Left InvalidSeeds
            else Right (unsafeSolanaPublicKeyRaw (BS.unpack bytes))

-- | Find the first bump seed (255 downto 0) whose derived address is off
-- the curve, mirroring the Rust SDK's @Pubkey::find_program_address@.
findProgramAddress :: [BS.ByteString] -> SolanaPublicKey -> Maybe (SolanaPublicKey, Word8)
findProgramAddress seeds programId = go 255
  where
    go :: Word8 -> Maybe (SolanaPublicKey, Word8)
    go bump =
      case createProgramAddress (seeds <> [BS.singleton bump]) programId of
        Right addr -> Just (addr, bump)
        Left InvalidSeeds
          | bump == 0 -> Nothing
          | otherwise -> go (bump - 1)
        Left _ -> Nothing

-- | A 32-byte string is "on curve" iff it decodes as a valid ed25519 point.
isOnCurve :: BS.ByteString -> Bool
isOnCurve bs =
  case Edwards.pointDecode (BA.convert bs :: BA.Bytes) of
    CryptoPassed _ -> True
    CryptoFailed _ -> False
