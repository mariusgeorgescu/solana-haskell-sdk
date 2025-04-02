{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
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
  void $ runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    -- Fund fee payer
    void $ requestAirdrop myPublicKey 10_000_000_000
    wait 15 -- Wait 15 seconds be sure the tx was confirmed

    -- Define recipient's address from base58 hex ecoded string
    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    -- Check balance
    printBalances [myPublicKey, recipient]

    -- Create a new transaction.
    _txId <-
      newTransaction
        [myPrivateKey] -- Signing keys (with all required signers)
        -- List of instructions
        [ SystemProgram.transfer
            myPublicKey -- sender address
            recipient -- recipient address
            1_000_000_000 -- amount to transfer 1 SOL
        ]

    wait 15 -- Wait 15 seconds be sure the tx was confirmed
    -- Check balance
    printBalances [myPublicKey, recipient]