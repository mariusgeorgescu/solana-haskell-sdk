module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import Core.Crypto
import Core.Transaction (newTransactionIntent)
import NativePrograms.SystemProgram qualified as SystemProgram
import Network.Web3.Provider
import RPC.HTTP
import RPC.Providers

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  (myPubKey1, myPrivKey1) <- createSolanaKeyPair

  (myPubKey2, myPrivKey2) <- createSolanaKeyPair

  -- let myPrivKey1 = unsafeSolanaPrivateKeyRaw [22, 92, 4, 245, 212, 153, 201, 54, 28, 97, 151, 138, 173, 130, 57, 22, 83, 124, 200, 60, 185, 35, 153, 114, 83, 74, 208, 151, 167, 200, 251, 120, 61, 127, 0, 228, 106, 0, 194, 74, 250, 228, 47, 77, 167, 219, 246, 233, 132, 0, 104, 179, 87, 70, 129, 53, 241, 74, 191, 236, 104, 204, 2, 113]
  -- let myPubKey1 = toSolanaPublicKey myPrivKey1
  -- let myPubKey2 = unsafeSolanaPublicKey "594C9C199Zp8fK2zvmXrSveE359gijrM6tsoZLYk9obv"

  print ("My keys :" :: String)
  print myPubKey1
  print myPrivKey1

  ret <- runWeb3' localHttpProvider $ do
    -- --
    liftIO $ putStrLn ("Requesting airdrop ..." :: String)
    r <- requestAirdrop myPubKey1 10_000_000_000
    liftIO $ putStrLn r
    liftIO $ threadDelay (15 * 1000000)
    liftIO $ putStrLn ("Checking balance ..." <> show myPubKey1)
    bal <- getBalance myPubKey1
    liftIO $ print (value bal)
    --
    liftIO $ putStrLn ("Requesting airdrop ..." :: String)
    r <- requestAirdrop myPubKey2 1_000_000_000
    liftIO $ print r
    liftIO $ threadDelay (15 * 1000000)
    liftIO $ putStrLn ("Checking balance ..." <> show myPubKey2)
    bal <- getBalance myPubKey2
    liftIO $ print (value bal)

    --
    liftIO $ putStrLn ("Getting latest blockhash ..." :: String)
    bh <- getLatestBlockhash
    liftIO $ print (value bh)

    let newTx =
          newTransactionIntent
            [myPrivKey1]
            [ SystemProgram.transfer myPubKey1 myPubKey2 1_000_000_000
            ]

    signedTx <- liftIO $ either throw return (newTx (value bh))
    liftIO $ putStrLn ("Signed tx: " <> signedTx)

    liftIO $ putStrLn "Submiting tx"
    tId <- sendTransaction signedTx
    liftIO $ putStrLn ("Transaction id: " <> tId)

    liftIO $ threadDelay (25 * 1000000)
    liftIO $ putStrLn ("Checking balance ..." <> show myPubKey1)
    bal <- getBalance myPubKey1
    liftIO $ print (value bal)
    liftIO $ putStrLn ("Checking balance ..." <> show myPubKey2)
    bal <- getBalance myPubKey2
    liftIO $ print (value bal)
  return ()
