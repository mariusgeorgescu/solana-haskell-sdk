-- |
-- Module      : Network.Solana.SolanaWeb3
-- Description : High-level helpers that bundle common Solana client workflows.
--
-- Convenience layer on top of the JSON-RPC bindings: build, sign, and submit a
-- transaction in one call, print account balances, and pause execution while
-- waiting for confirmation. Intended for scripts and demos; for finer control
-- over blockhash selection, error handling, and confirmation strategy use
-- "Network.Solana.Core.Message" and "Network.Solana.RPC.HTTP.Transaction"
-- directly.
--
-- Also bundles typed fetch helpers ('getTokenAccount', 'getMint',
-- 'getLookupTable', 'getStakeAccount', 'getNonceAccount',
-- 'getMetadataAccount') that fetch an account over RPC and decode its state
-- with the corresponding on-chain-state decoder, so callers don't have to
-- extract and decode 'Network.Solana.Core.Account.AccountData' by hand; like
-- every other helper in this module, they require a reachable RPC node and
-- are compile-verified only.
module Network.Solana.SolanaWeb3 where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isNothing)
import Network.Solana.Core.Account (AccountData (..), AccountInfo (dataField))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message (newTransactionIntent)
import Network.Solana.Core.VersionedMessage qualified as VM
import Network.Solana.Metaplex.TokenMetadata qualified as TM
import Network.Solana.NativePrograms.AddressLookupTable qualified as ALT
import Network.Solana.NativePrograms.Stake qualified as Stake
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.RPC.HTTP.Account (confirmationStatusTxStatus, errTxStatus, getAccountInfo, getBalance, getSignatureStatuses)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Transaction
import Network.Solana.SplPrograms.Token qualified as Tok
import Network.Web3 hiding (AccountData)

-- | Builds, signs, and submits a transaction that executes the given
-- instructions, returning its signature.
--
-- Fetches the latest blockhash from the cluster, compiles the instructions
-- into a message, signs it with the given keys (the first key is the fee
-- payer), and broadcasts the result with 'sendTransaction'.
--
-- Throws a 'CompileException' if an instruction references an account key
-- that cannot be resolved in the compiled message.
newTransaction :: [SolanaPrivateKey] -> [Instruction] -> Web3 SolanaSignature
newTransaction signers instructions = do
  let newTxInt = newTransactionIntent signers instructions
  bh <- getTheLatestBlockhash
  signedTx <- either (liftIO . throwIO) pure (newTxInt bh)
  sendTransaction signedTx

-- | Prints the balance of each of the given accounts to standard output using 'printBalance'.
printBalances :: [SolanaPublicKey] -> Web3 ()
printBalances = mapM_ printBalance

-- | Fetches the balance of the given account with 'getBalance' and prints it to standard output.
printBalance :: SolanaPublicKey -> Web3 ()
printBalance addr = do
  balance <- getBalance addr
  liftIO $ putStrLn $ "Balance for " <> show addr <> " is: " <> show balance

-- | Suspends execution for the given number of seconds, printing a notice first.
-- A crude way to wait for transaction confirmation; prefer polling
-- 'Network.Solana.RPC.HTTP.Account.getSignatureStatuses' in real applications.
wait :: Int -> Web3 ()
wait s = liftIO $ do
  putStrLn ("Wait " <> show s <> " seconds to make sure tx is confirmed ..")
  threadDelay (s * 1000000)

-- | Polls 'Network.Solana.RPC.HTTP.Account.getSignatureStatuses' once a
-- second, up to 30 times, until the given signature reaches @confirmed@ or
-- @finalized@ status.
--
-- Returns 'True' only if the status reaches the target commitment /and/ the
-- node reports no on-chain error for it (its @err@ field is 'Nothing'); a
-- transaction that landed but failed on-chain (e.g. a program error) yields
-- 'False', not 'True'. Also returns 'False' if it does not confirm within
-- the poll budget.
confirmTransaction :: SolanaSignature -> Web3 Bool
confirmTransaction sig = go (30 :: Int)
  where
    go 0 = pure False
    go n = do
      statuses <- getSignatureStatuses [sig]
      case statuses of
        [Just s]
          | confirmationStatusTxStatus s `elem` [Just "confirmed", Just "finalized"] ->
              pure (isNothing (errTxStatus s))
        [_] -> wait 1 >> go (n - 1)
        _ -> liftIO . throwIO . userError $
          "confirmTransaction: expected exactly one status for one signature, got " <> show (length statuses)

------------------------------------------------------------------------------------------------

-- * Account-state fetch helpers

------------------------------------------------------------------------------------------------

-- | Extracts the raw bytes backing an account's @data@ field. Every fetch
-- helper in this module requests base64 encoding (see
-- 'Network.Solana.RPC.HTTP.Account.getAccountInfo'), so
-- 'Network.Solana.Core.Account.AccountDataJSON' should never occur in
-- practice; if it does, it is reported the same way as any other decode
-- failure.
accountDataBytes :: AccountData -> Either String BS.ByteString
accountDataBytes (AccountDataBinary bs) = Right bs
accountDataBytes (AccountDataJSON _) = Left "account data was not base64-encoded"

-- | Fetches the given account with 'Network.Solana.RPC.HTTP.Account.getAccountInfo'
-- and decodes its data with the given decoder, prefixing any failure message
-- with the given label.
--
-- Returns 'Nothing' if the account does not exist. Propagates the underlying
-- JSON-RPC exception on RPC failure, same as every other RPC-backed helper in
-- this module. Throws @'userError' (label <> ": state decode failed: " <> err)@
-- if the account exists but its data cannot be decoded -- distinct from the
-- not-found case.
fetchAccountState :: String -> (BS.ByteString -> Either String a) -> SolanaPublicKey -> Web3 (Maybe a)
fetchAccountState label decodeFn addr = do
  macc <- getAccountInfo addr
  case macc of
    Nothing -> pure Nothing
    Just acc ->
      either
        (liftIO . throwIO . userError . ((label <> ": state decode failed: ") <>))
        (pure . Just)
        (accountDataBytes (dataField acc) >>= decodeFn)

-- | Fetches the given account and decodes it as an SPL Token account with
-- 'Tok.decodeTokenAccount'.
--
-- Returns 'Nothing' if the account does not exist. Throws on RPC failure, per
-- 'fetchAccountState'. Throws @'userError'
-- ("getTokenAccount: state decode failed: " <> err)@ if the account exists
-- but is not a well-formed SPL Token account -- distinct from the not-found
-- case.
getTokenAccount :: SolanaPublicKey -> Web3 (Maybe Tok.TokenAccount)
getTokenAccount = fetchAccountState "getTokenAccount" Tok.decodeTokenAccount

-- | Fetches the given account and decodes it as an SPL Token mint with
-- 'Tok.decodeMint'.
--
-- Returns 'Nothing' if the account does not exist. Throws on RPC failure, per
-- 'fetchAccountState'. Throws @'userError'
-- ("getMint: state decode failed: " <> err)@ if the account exists but is
-- not a well-formed mint -- distinct from the not-found case.
getMint :: SolanaPublicKey -> Web3 (Maybe Tok.Mint)
getMint = fetchAccountState "getMint" Tok.decodeMint

-- | Fetches the given address lookup table account, decodes it with
-- 'ALT.decodeLookupTable', and bridges it into the
-- 'VM.AddressLookupTableAccount' shape 'Network.Solana.Core.VersionedMessage.compileV0Message'
-- expects (via 'ALT.lookupTableToAccount'), using the queried address as the
-- table's own key.
--
-- Returns 'Nothing' if the account does not exist. Throws on RPC failure, per
-- 'fetchAccountState'. Throws @'userError'
-- ("getLookupTable: state decode failed: " <> err)@ if the account exists but
-- is not a well-formed lookup table -- distinct from the not-found case.
getLookupTable :: SolanaPublicKey -> Web3 (Maybe VM.AddressLookupTableAccount)
getLookupTable key = do
  mState <- fetchAccountState "getLookupTable" ALT.decodeLookupTable key
  pure (ALT.lookupTableToAccount key <$> mState)

-- | Fetches the given account and decodes it as a stake account with
-- 'Stake.decodeStakeAccount'.
--
-- Returns 'Nothing' if the account does not exist. Throws on RPC failure, per
-- 'fetchAccountState'. Throws @'userError'
-- ("getStakeAccount: state decode failed: " <> err)@ if the account exists
-- but is not a well-formed stake account -- distinct from the not-found case.
getStakeAccount :: SolanaPublicKey -> Web3 (Maybe Stake.StakeState)
getStakeAccount = fetchAccountState "getStakeAccount" Stake.decodeStakeAccount

-- | Fetches the given account and decodes it as a nonce account with
-- 'SP.decodeNonceAccount'.
--
-- Returns 'Nothing' if the account does not exist. Throws on RPC failure, per
-- 'fetchAccountState'. Throws @'userError'
-- ("getNonceAccount: state decode failed: " <> err)@ if the account exists
-- but is not a well-formed nonce account -- distinct from the not-found case.
getNonceAccount :: SolanaPublicKey -> Web3 (Maybe SP.NonceState)
getNonceAccount = fetchAccountState "getNonceAccount" SP.decodeNonceAccount

-- | Fetches the Metaplex metadata account for the given mint (deriving its
-- PDA with 'TM.deriveMetadataAddress') and decodes it with
-- 'TM.decodeMetadata'.
--
-- Calls 'error' if the metadata PDA cannot be derived, matching the
-- precedent set by 'Network.Solana.Metaplex.TokenMetadata.createMetadataAccountV3'
-- (practically unreachable: requires every bump candidate to land on-curve).
-- Otherwise, returns 'Nothing' if the metadata account does not exist.
-- Throws on RPC failure, per 'fetchAccountState'. Throws @'userError'
-- ("getMetadataAccount: state decode failed: " <> err)@ if the account
-- exists but is not well-formed metadata -- distinct from the not-found case.
getMetadataAccount :: SolanaPublicKey -> Web3 (Maybe TM.Metadata)
getMetadataAccount mint =
  fetchAccountState "getMetadataAccount" TM.decodeMetadata pda
  where
    pda = fromMaybe (error "getMetadataAccount: metadata PDA derivation failed") (TM.deriveMetadataAddress mint)