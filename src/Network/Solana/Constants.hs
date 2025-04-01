-- |
-- Module      : Network.Solana.Constants
-- Description : Useful constants for working with the Solana blockchain.
--
-- This module provides well-known constants related to Solana's economic model,
-- such as the number of lamports (the smallest unit) in one SOL token.
module Network.Solana.Constants where

-- | Number of lamports in one SOL.
--
-- One SOL (the native token of the Solana blockchain) is equal to 1,000,000,000 lamports.
-- Lamports are the smallest indivisible unit of SOL, similar to "satoshis" in Bitcoin , "wei" in Ethereum or 'lovelaces' in Cardano.
lamportsPerSol :: Integer
lamportsPerSol = 1_000_000_000
