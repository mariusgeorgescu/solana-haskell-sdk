-- | Shared plumbing for the local-validator integration suite.
--
-- Every helper here runs against a real 'Web3' RPC connection (see 'run')
-- rather than fixtures, so tests built on top of this module only pass
-- against a live @solana-test-validator@ (or another reachable cluster).
--
-- Commitment discipline: the node's default commitment is @finalized@, and
-- so is the default preflight commitment used for submitted transactions.
-- 'fundedKeypair' and 'fundedKeypairs' therefore finalize their airdrops
-- before returning (later sends preflight against the finalized bank), and
-- 'sendAndConfirm' pins preflight at @confirmed@ so multi-step flows can
-- build on merely-confirmed intermediate state. Callers are expected to
-- 'confirmFinalized' the last transaction of a flow before asserting on
-- account state or balances (finalization is monotone by slot, so one wait
-- covers every prior transaction).
module Test.Integration.Setup
  ( rpcUrl,
    run,
    fundedKeypair,
    fundedKeypairs,
    confirmOrFail,
    confirmFinalized,
    sendAndConfirm,
    requireJust,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Network.Solana.Core.Account (Lamport)
import Network.Solana.Core.Crypto (SolanaPrivateKey, SolanaPublicKey, SolanaSignature, createSolanaKeyPair, toSolanaPublicKey)
import Network.Solana.Core.Instruction (Instruction)
import Network.Solana.Core.Message (newTransactionIntentWithPayer)
import Network.Solana.RPC.HTTP.Account (confirmationStatusTxStatus, errTxStatus, getSignatureStatuses)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Transaction (requestAirdrop, sendTransaction')
import Network.Solana.RPC.HTTP.Types (defaultConfigObject, encoding, preflightCommitment)
import Network.Solana.SolanaWeb3 (confirmTransaction)
import Network.Web3.Provider (Provider (HttpProvider), Web3, runWeb3')
import System.Environment (lookupEnv)

-- | The RPC endpoint under test: @SOLANA_RPC_URL@ if set, otherwise the
-- default local validator address.
rpcUrl :: IO String
rpcUrl = fromMaybe "http://127.0.0.1:8899" <$> lookupEnv "SOLANA_RPC_URL"

-- | Runs a 'Web3' action against 'rpcUrl', throwing a 'userError' describing
-- the underlying 'Network.Web3.Provider.Web3Error' on RPC failure.
run :: Web3 a -> IO a
run action = do
  url <- rpcUrl
  result <- runWeb3' (HttpProvider url) action
  either (throwIO . userError . ("web3 error: " <>) . show) pure result

-- | Generates a fresh random keypair and airdrops it the given amount,
-- waiting for the airdrop to reach @finalized@ commitment before returning.
--
-- Later sends preflight against the finalized bank, so funds must be
-- finalized before the first spend.
fundedKeypair :: Lamport -> Web3 (SolanaPublicKey, SolanaPrivateKey)
fundedKeypair amount = do
  (pk, sk) <- liftIO createSolanaKeyPair
  sig <- requestAirdrop pk amount
  confirmFinalized sig
  pure (pk, sk)

-- | Generates fresh random keypairs and airdrops each the corresponding
-- amount. Airdrops are all requested before any is finalized, so the waits
-- for each signature overlap instead of running back-to-back.
fundedKeypairs :: [Lamport] -> Web3 [(SolanaPublicKey, SolanaPrivateKey)]
fundedKeypairs amounts = do
  pairs <- liftIO (mapM (const createSolanaKeyPair) amounts)
  sigs <- mapM (\(amount, (pk, _)) -> requestAirdrop pk amount) (zip amounts pairs)
  mapM_ confirmFinalized sigs
  pure pairs

-- | Confirms the given signature with 'confirmTransaction' (reaches
-- @confirmed@ or @finalized@ with no on-chain error), throwing a 'userError'
-- naming the given label and the signature if it does not.
confirmOrFail :: String -> SolanaSignature -> Web3 ()
confirmOrFail what sig = do
  ok <- confirmTransaction sig
  if ok
    then pure ()
    else liftIO . throwIO . userError $ what <> " failed to confirm: " <> show sig

-- | Polls 'getSignatureStatuses' once a second, up to 60 times, until the
-- given signature reaches @finalized@ status.
--
-- Throws a 'userError' if the transaction finalizes with an on-chain error,
-- or if it does not finalize within the poll budget (naming the signature in
-- both cases).
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

-- | Compiles the given signers and instructions with
-- 'newTransactionIntentWithPayer', naming the first signer as the fee payer
-- (forced writable regardless of what the given instructions themselves
-- declare -- plain 'newTransactionIntent' derives writability from the
-- instructions alone, which leaves the sole signer of an authority-only
-- instruction such as 'Network.Solana.SplPrograms.Token.mintTo' or
-- 'Network.Solana.SplPrograms.Token.transferChecked' non-writable, and every
-- Solana transaction requires its fee payer to be writable). Sends the
-- result with preflight pinned at @confirmed@ (multi-step flows build on
-- merely-confirmed state; the default preflight at @finalized@ would reject
-- them), and confirms it with 'confirmOrFail'.
sendAndConfirm :: [SolanaPrivateKey] -> [Instruction] -> Web3 SolanaSignature
sendAndConfirm [] _ = liftIO (throwIO (userError "sendAndConfirm: no signers given"))
sendAndConfirm signers@(feePayer : _) ixs = do
  bh <- getTheLatestBlockhash
  tx <- either (liftIO . throwIO) pure (newTransactionIntentWithPayer (toSolanaPublicKey feePayer) signers ixs bh)
  sig <- sendTransaction' tx (defaultConfigObject {encoding = Just "base64", preflightCommitment = Just "confirmed"})
  confirmOrFail "transaction" sig
  pure sig

-- | Unwraps a 'Just', throwing a 'userError' naming the given label if it is
-- 'Nothing'.
requireJust :: String -> Maybe a -> Web3 a
requireJust what = maybe (liftIO (throwIO (userError (what <> ": expected Just")))) pure
