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
-- extract and decode 'Network.Solana.Core.Account.AccountData' by hand. Like
-- every other helper in this module, they require a reachable RPC node, so
-- the unit suite only compiles them; live coverage comes from the opt-in
-- local-validator integration suite (@cabal test integration-tests
-- --flags=integration@, see the README), which exercises most but not all of
-- them.
module Network.Solana.SolanaWeb3 where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word64)
import Network.Solana.Core.Account (AccountData (..), AccountInfo (dataField))
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message (newDurableNonceTransactionIntentWithPayer, newTransactionIntentWithPayer)
import Network.Solana.Core.VersionedMessage qualified as VM
import Network.Solana.Metaplex.TokenMetadata qualified as TM
import Network.Solana.NativePrograms.AddressLookupTable qualified as ALT
import Network.Solana.NativePrograms.Stake qualified as Stake
import Network.Solana.NativePrograms.SystemProgram qualified as SP
import Network.Solana.RPC.HTTP.Account (confirmationStatusTxStatus, errTxStatus, getAccountInfo, getAccountInfo', getBalance, getSignatureStatuses)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Chain (PrioritizationFee (prioritizationFee), getRecentPrioritizationFees, percentilePriorityFee)
import Network.Solana.RPC.HTTP.Transaction
import Network.Solana.RPC.HTTP.Types (ConfigurationObject, cfgJustEncodingBase64, commitment, preflightCommitment, value)
import Network.Solana.SplPrograms.Token qualified as Tok
import Network.Web3 hiding (AccountData, value)

-- | Builds, signs, and submits a transaction that executes the given
-- instructions, returning its signature.
--
-- Fetches the latest blockhash from the cluster, compiles the instructions
-- with 'newTransactionIntentWithPayer' naming the first key as the fee
-- payer (forced into account 0 as a writable signer, even if no instruction
-- marks it writable -- e.g. a memo-only or SPL authority-only transaction
-- -- or references it at all), signs with the given keys (any order;
-- signatures are placed in message order automatically), and broadcasts
-- the result with 'sendTransaction'.
--
-- Throws @'userError' "newTransaction: no signers given"@ if the key list
-- is empty, and a 'CompileException' if an instruction references an
-- account key that cannot be resolved in the compiled message, if a
-- required signer has no corresponding key, or if a given key signs
-- nothing.
newTransaction :: [SolanaPrivateKey] -> [Instruction] -> Web3 SolanaSignature
newTransaction [] _ = liftIO . throwIO . userError $ "newTransaction: no signers given"
newTransaction signers@(payer : _) instructions = do
  bh <- getTheLatestBlockhash
  signedTx <-
    either
      (liftIO . throwIO)
      pure
      (newTransactionIntentWithPayer (toSolanaPublicKey payer) signers instructions bh)
  sendTransaction signedTx

-- | Configuration used to fetch the nonce account in 'newNonceTransaction':
-- base64 encoding (needed to decode the account's data, same as
-- 'Network.Solana.RPC.HTTP.Account.getAccountInfo') plus an explicit
-- @confirmed@ commitment, so a recently created or recently advanced nonce
-- account is not missed by the node's default @finalized@ commitment.
cfgNonceAccountConfirmed :: ConfigurationObject
cfgNonceAccountConfirmed = cfgJustEncodingBase64 {commitment = Just "confirmed"}

-- | Configuration used to submit the transaction in 'newNonceTransaction':
-- 'defaultRpcSendTransactionConfig' with the preflight simulation pinned at
-- the commitment the nonce account is read at ('cfgNonceAccountConfirmed').
-- The node's default @finalized@ preflight bank may not yet hold the nonce
-- value the transaction was built against (or the account at all) right
-- after creation or an advance, and rejects the send with @BlockhashNotFound@.
cfgNonceSendConfirmed :: ConfigurationObject
cfgNonceSendConfirmed = defaultRpcSendTransactionConfig {preflightCommitment = commitment cfgNonceAccountConfirmed}

-- | Builds, signs, and submits a durable-nonce transaction with an
-- explicitly named fee payer that executes the given instructions,
-- returning its signature.
--
-- Reads the given nonce account at @confirmed@ commitment to avoid
-- stale-nonce races (a nonce account fetched at the node's default
-- @finalized@ commitment may appear not-found or stale immediately after
-- creation or after being advanced), then builds the transaction with
-- 'newDurableNonceTransactionIntentWithPayer', which prepends the
-- nonce-advance instruction ahead of the given instructions and uses the
-- nonce account's durable nonce in place of a recent block hash (so the
-- transaction never expires, unlike 'newTransaction'), and submits it with
-- 'sendTransaction'' under 'cfgNonceSendConfirmed', so the node's preflight
-- simulation runs at that same @confirmed@ commitment instead of its default
-- @finalized@ bank (which may not yet contain the nonce value used, or the
-- account at all, immediately after creation or an advance). The fee payer is
-- always account 0 (sponsored fees are supported), and the given signers
-- may be listed in any order — they must include private keys
-- corresponding to both the payer and the nonce account's authority
-- (@nsAuthority@), or the resulting transaction will be rejected on
-- submission. Requires a reachable RPC node.
--
-- Throws @'userError' "newNonceTransaction: nonce account not found"@ if
-- the account does not exist, and @'userError' "newNonceTransaction: nonce
-- account not initialized"@ if it exists but has not been initialized as a
-- nonce account -- distinct from each other and from the 'CompileException'
-- thrown if an instruction references an account key that cannot be
-- resolved in the compiled message.
newNonceTransaction :: SolanaPublicKey -> [SolanaPrivateKey] -> SolanaPublicKey -> [Instruction] -> Web3 SolanaSignature
newNonceTransaction payer signers nonceAccount instructions = do
  mAccInfo <- value <$> getAccountInfo' nonceAccount cfgNonceAccountConfirmed
  case mAccInfo of
    Nothing -> liftIO . throwIO . userError $ "newNonceTransaction: nonce account not found"
    Just acc -> do
      nonceState <-
        either
          (liftIO . throwIO . userError . ("newNonceTransaction: state decode failed: " <>))
          pure
          (accountDataBytes (dataField acc) >>= SP.decodeNonceAccount)
      case nonceState of
        SP.NonceUninitialized -> liftIO . throwIO . userError $ "newNonceTransaction: nonce account not initialized"
        SP.NonceInitialized authority durableNonce _ -> do
          let newTxInt = newDurableNonceTransactionIntentWithPayer payer signers nonceAccount authority instructions
          signedTx <- either (liftIO . throwIO) pure (newTxInt durableNonce)
          sendTransaction' signedTx cfgNonceSendConfirmed

-- | Estimates a priority fee (in micro-lamports per compute unit) for
-- landing a transaction that writes to the given accounts, as the @p@-th
-- percentile of recent per-compute-unit fees observed for them (via
-- 'getRecentPrioritizationFees' and 'percentilePriorityFee').
--
-- Heuristic only: recent fees are no guarantee of what is needed to land
-- the next transaction. Returns @0@ if no recent priority fees were
-- observed for the given accounts. Requires a reachable RPC node.
estimatePriorityFee :: [SolanaPublicKey] -> Double -> Web3 Word64
estimatePriorityFee addresses p = do
  fees <- getRecentPrioritizationFees (Just addresses)
  pure (percentilePriorityFee p (map prioritizationFee fees))

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
-- the poll budget. See 'confirmFinalized' to wait for finalization instead.
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

-- | Polls 'Network.Solana.RPC.HTTP.Account.getSignatureStatuses' once a
-- second, up to 60 times, until the given signature reaches @finalized@
-- status.
--
-- Throws a 'userError' if the transaction finalizes with an on-chain error,
-- or if it does not finalize within the poll budget (naming the signature in
-- both cases). Use it before reading state through helpers that use the
-- node's default commitment ('printBalances',
-- 'Network.Solana.RPC.HTTP.Account.getBalance', 'getTokenAccount', ...): a
-- transaction that 'confirmTransaction' reports at @confirmed@ is not yet
-- visible there.
confirmFinalized :: SolanaSignature -> Web3 ()
confirmFinalized sig = go (60 :: Int)
  where
    go 0 = liftIO . throwIO . userError $ "confirmFinalized: timed out waiting for finalization of " <> show sig
    go n = do
      statuses <- getSignatureStatuses [sig]
      case statuses of
        [Just s] | confirmationStatusTxStatus s == Just "finalized" ->
          case errTxStatus s of
            Nothing -> pure ()
            Just err -> liftIO . throwIO . userError $ "confirmFinalized: " <> show sig <> " failed on-chain: " <> show err
        _ -> liftIO (threadDelay 1000000) >> go (n - 1)

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
accountDataBytes AccountDataJSON {} = Left "account data was returned jsonParsed, not as bytes"

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