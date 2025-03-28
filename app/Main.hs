{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Core.Crypto
import Network.Web3.Provider
import RPC.HTTP (getLatestBlockhash)
import RPC.Providers

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  let user1 = unsafeSolanaPublicKey "3gfNF1r7CUow2SdVj9L8j66KAdZFBqvsgYUB7GH7cSNs"
  ret <- runWeb3' devnetHttpProvider $ do
    bal <- getBalance "594C9C199Zp8fK2zvmXrSveE359gijrM6tsoZLYk9obv"
    liftIO $ print (value bal)

    bh <- getLatestBlockhash
    liftIO $ print (value bh)

    return ()

  return ()
