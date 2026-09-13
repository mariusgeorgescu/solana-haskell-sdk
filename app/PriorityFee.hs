{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.ComputeBudget qualified as ComputeBudget
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Transaction (requestAirdrop)
import Network.Solana.SolanaWeb3
import Network.Solana.SplPrograms.Memo qualified as Memo
import Network.Web3.Provider

main :: IO ()
main = do
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair

  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4"

    txId <-
      newTransaction
        [myPrivateKey]
        [ ComputeBudget.setComputeUnitLimit 200_000, -- cap the compute units this tx may use
          ComputeBudget.setComputeUnitPrice 10_000, -- priority fee: micro-lamports per compute unit
          SystemProgram.transfer myPublicKey recipient 1_000_000_000, -- 1 SOL
          Memo.buildMemo "thanks for the coffee" [myPublicKey] -- signed on-chain note
        ]
    confirmFinalized txId
    printBalances [recipient]

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
