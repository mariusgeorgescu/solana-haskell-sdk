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
module Network.Solana.SolanaWeb3 where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message (newTransactionIntent)
import Network.Solana.RPC.HTTP.Account (confirmationStatusTxStatus, getBalance, getSignatureStatuses)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Transaction
import Network.Web3

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
-- @finalized@ status. Returns 'False' if it does not confirm in time.
confirmTransaction :: SolanaSignature -> Web3 Bool
confirmTransaction sig = go (30 :: Int)
  where
    go 0 = pure False
    go n = do
      [st] <- getSignatureStatuses [sig]
      case st of
        Just s | confirmationStatusTxStatus s `elem` [Just "confirmed", Just "finalized"] -> pure True
        _ -> wait 1 >> go (n - 1)