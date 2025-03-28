module RPC.Providers
  ( devnetHttpProvider,
    localHttpProvider,
  )
where

import Network.Web3.Provider

devnetHttpProvider = (HttpProvider "https://api.devnet.solana.com")

localHttpProvider = (HttpProvider "http://127.0.0.1:8899")