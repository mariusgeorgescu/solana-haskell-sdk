{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Transaction
import Network.Solana.SolanaWeb3
import Network.Web3.Provider

main :: IO ()
main = do
  -- Generate keypairs for fee payer (sender)
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair

  -- Create Connection, local validator in this example
  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    -- Fund the fee payer and wait until the airdrop is finalized: sends are
    -- preflighted against the finalized bank, and balances are read there too
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    -- Define recipient's address from a base58-encoded string
    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    -- Check balance
    printBalances [myPublicKey, recipient]

    -- Create a new transaction.
    txId <-
      newTransaction
        [myPrivateKey] -- Signing keys (the first key pays the fee)
        -- List of instructions
        [ SystemProgram.transfer
            myPublicKey -- sender address
            recipient -- recipient address
            1_000_000_000 -- amount to transfer 1 SOL
        ]
    liftIO $ putStrLn ("Transaction sent: " <> show txId)

    -- Wait for finalization (throws if the transaction failed on-chain or
    -- timed out), so the balances below reflect the transfer
    confirmFinalized txId
    printBalances [myPublicKey, recipient]

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
