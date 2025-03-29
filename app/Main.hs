module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Crypto
import Core.Message (newTransactionIntent)
import NativePrograms.SystemProgram qualified as SystemProgram
import Network.Web3.Provider
import RPC.HTTP
import RPC.Providers

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  (myPubKey1, myPrivKey1) <- createSolanaKeyPair

  (myPubKey2, myPrivKey2) <- createSolanaKeyPair

  print ("My keys :" :: String)
  print myPubKey1
  print myPrivKey1

  ret <- runWeb3' localHttpProvider $ do
    bal <- getBalance $ unsafeSolanaPublicKey "594C9C199Zp8fK2zvmXrSveE359gijrM6tsoZLYk9obv"
    liftIO $ print (value bal)

    --
    liftIO $ putStrLn ("Requesting airdrop ..." :: String)
    r <- requestAirdrop myPubKey1 10_000_000_000
    liftIO $ putStrLn r
    liftIO $ threadDelay (15 * 1000000)
    liftIO $ putStrLn ("Checking balance ..." :: String)
    bal <- getBalance myPubKey1
    liftIO $ print (value bal)
    --
    liftIO $ putStrLn ("Requesting airdrop ..." :: String)
    r <- requestAirdrop myPubKey2 1_000_000_000
    liftIO $ print r
    liftIO $ threadDelay (15 * 1000000)
    liftIO $ putStrLn ("Checking balance ..." :: String)
    bal <- getBalance myPubKey2
    liftIO $ print (value bal)

    --
    liftIO $ putStrLn ("Getting latest blockhash ..." :: String)
    bh <- getLatestBlockhash
    liftIO $ print (value bh)

    let newTx =
          newTransactionIntent
            [myPrivKey1]
            [ SystemProgram.transfer myPubKey1 myPubKey2 5_000_000_000
            ]

    let signedTx = newTx (value bh)
    liftIO $ putStrLn ("Signed tx :" :: String)
    liftIO $ print signedTx
    return ()

  return ()
