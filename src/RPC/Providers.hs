module RPC.Providers
  ( devnetHttpProvider,
    localHttpProvider,
  )
where

import Network.Web3.Provider

devnetHttpProvider :: Provider
devnetHttpProvider = HttpProvider "https://api.devnet.solana.com"

localHttpProvider :: Provider
localHttpProvider = HttpProvider "http://127.0.0.1:8899"