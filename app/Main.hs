{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Crypto
import Network.Web3.Provider
import RPC.HTTP
import RPC.HTTP (getBalance)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  let user1 = unsafeSolanaPublicKey "3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs"
  ret <- runWeb3' (HttpProvider "https://api.devnet.solana.com") $ do
    bal <- getBalance "3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs"
    liftIO $ print (value bal)
    return $ (value bal)

  return ()