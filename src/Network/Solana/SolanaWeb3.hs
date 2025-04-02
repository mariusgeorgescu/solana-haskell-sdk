module Network.Solana.SolanaWeb3 where

import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad.IO.Class
import Network.Solana.Core.Crypto
import Network.Solana.Core.Instruction
import Network.Solana.Core.Message (newTransactionIntent)
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Transaction
import Network.Web3

newTransaction :: [SolanaPrivateKey] -> [Instruction] -> Web3 SolanaSignature
newTransaction signers instructions = do
  let newTxInt = newTransactionIntent signers instructions
  bh <- getTheLatestBlockhash
  let signedTx = either throw id (newTxInt bh)
  sendTransaction signedTx

printBalances :: [SolanaPublicKey] -> Web3 ()
printBalances = mapM_ printBalance

printBalance :: SolanaPublicKey -> Web3 ()
printBalance addr = do
  balance <- getBalance addr
  liftIO $ putStrLn $ "Balance for " <> show addr <> " is :" <> show balance

wait :: Int -> Web3 ()
wait s = liftIO $ do
  putStrLn ("Wait " <> show s <> " seconds to make sure tx is confirmed ..")
  threadDelay (s * 1000000)