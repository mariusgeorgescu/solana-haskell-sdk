{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Network.Solana.Core.Crypto
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Tokenomics (getMinimumBalanceForRentExemption)
import Network.Solana.RPC.HTTP.Transaction (requestAirdrop)
import Network.Solana.SolanaWeb3
import Network.Solana.SplPrograms.AssociatedTokenAccount qualified as Ata
import Network.Solana.SplPrograms.Token qualified as Token
import Network.Web3.Provider

main :: IO ()
main = do
  (myPublicKey, myPrivateKey) <- createSolanaKeyPair
  (mint, mintPrivateKey) <- createSolanaKeyPair -- the new token's mint account

  result <- runWeb3' (HttpProvider "http://127.0.0.1:8899") $ do
    requestAirdrop myPublicKey 10_000_000_000 >>= confirmFinalized

    let recipient = "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9XUAUqV4" -- recipient wallet

        -- ATAs are PDAs of (wallet, token program, mint): derived, not generated.
        sourceAta = fromJust (Ata.getAssociatedTokenAddress myPublicKey mint)
        destinationAta = fromJust (Ata.getAssociatedTokenAddress recipient mint)

    -- Setup: create a mint with 6 decimals (we are its mint authority), our own
    -- token account, and mint 10 tokens into it. The mint keypair co-signs
    -- because createAccount requires the new account's signature.
    mintRent <- getMinimumBalanceForRentExemption 82 -- a mint account is 82 bytes
    setupTx <-
      newTransaction
        [myPrivateKey, mintPrivateKey]
        [ SystemProgram.createAccount myPublicKey mint mintRent 82 Token.tokenProgramId,
          Token.initializeMint2 mint 6 myPublicKey Nothing,
          Ata.createAssociatedTokenAccount myPublicKey myPublicKey mint,
          Token.mintTo mint sourceAta myPublicKey [] 10_000_000
        ]
    confirmFinalized setupTx

    -- Transfer 1 token to the recipient
    transferTx <-
      newTransaction
        [myPrivateKey]
        [ -- Create the recipient's token account if it does not exist yet (no-op otherwise).
          Ata.createAssociatedTokenAccountIdempotent
            myPublicKey -- funder (pays rent)
            recipient -- wallet that will own the ATA
            mint,
          -- Transfer 1 token (6 decimals); mint and decimals are verified on-chain.
          Token.transferChecked
            sourceAta -- source token account
            mint -- token mint
            destinationAta -- destination token account
            myPublicKey -- owner of the source account
            [] -- extra multisig signers (none)
            1_000_000 -- amount in base units
            6 -- decimals of the mint
        ]
    confirmFinalized transferTx

    -- Read back the recipient's token account (decoded on-chain state)
    account <- getTokenAccount destinationAta
    liftIO $ putStrLn ("Recipient token balance: " <> show (Token.taAmount <$> account))

  either (\e -> putStrLn ("RPC error: " <> show e)) pure result
