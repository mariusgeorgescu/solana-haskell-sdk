{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import Core.Block (unsafeBlockHash)
import Core.Crypto
import Core.Message (newMessage, newMessageToBase64String, newTransactionIntent)
import Data.Either (fromRight)
import Data.Functor (void)
import NativePrograms.SystemProgram qualified as SystemProgram
import Network.Web3.Provider
import RPC.HTTP
import RPC.Providers
import RPC.Types (RPCResponse (..))

testTx :: IO ()
testTx = void $ runWeb3' localHttpProvider $ do
  (myPubKey1, myPrivKey1) <- liftIO createSolanaKeyPair
  (myPubKey2, myPrivKey2) <- liftIO createSolanaKeyPair

  liftIO $ putStrLn ("Requesting airdrop ..." :: String)
  r <- requestAirdrop myPubKey1 10_000_000_000
  liftIO $ putStrLn r
  liftIO $ threadDelay (15 * 1000000)
  liftIO $ putStrLn ("Checking balance ..." <> show myPubKey1)
  bal <- getBalance myPubKey1
  liftIO $ print bal
  --
  liftIO $ putStrLn ("Requesting airdrop ..." :: String)
  r <- requestAirdrop myPubKey2 1_000_000_000
  liftIO $ print r
  liftIO $ threadDelay (15 * 1000000)
  liftIO $ putStrLn ("Checking balance ..." <> show myPubKey2)
  bal <- getBalance myPubKey2
  liftIO $ print bal

  --
  liftIO $ putStrLn ("Getting latest blockhash ..." :: String)
  bh <- getTheLatestBlockhash
  liftIO $ print bh

  let newTx =
        newTransactionIntent
          [myPrivKey1]
          [ SystemProgram.transfer myPubKey1 myPubKey2 1_000_000_000
          ]

  signedTx <- liftIO $ either throw return (newTx bh)
  liftIO $ putStrLn ("Signed tx: " <> signedTx)

  liftIO $ putStrLn "Submiting tx"
  tId <- sendTransaction signedTx
  liftIO $ putStrLn ("Transaction id: " <> tId)

  liftIO $ threadDelay (25 * 1000000)
  liftIO $ putStrLn ("Checking balance ..." <> show myPubKey1)
  bal <- getBalance myPubKey1
  liftIO $ print bal
  liftIO $ putStrLn ("Checking balance ..." <> show myPubKey2)
  bal <- getBalance myPubKey2
  liftIO $ print bal

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  -- (myPubKey1, myPrivKey1) <- createSolanaKeyPair

  -- (myPubKey2, myPrivKey2) <- createSolanaKeyPair

  let myPrivKey1 = unsafeSolanaPrivateKeyRaw [22, 92, 4, 245, 212, 153, 201, 54, 28, 97, 151, 138, 173, 130, 57, 22, 83, 124, 200, 60, 185, 35, 153, 114, 83, 74, 208, 151, 167, 200, 251, 120, 61, 127, 0, 228, 106, 0, 194, 74, 250, 228, 47, 77, 167, 219, 246, 233, 132, 0, 104, 179, 87, 70, 129, 53, 241, 74, 191, 236, 104, 204, 2, 113]
  let myPubKey1 = toSolanaPublicKey myPrivKey1
  let myPubKey2 = unsafeSolanaPublicKey "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9EUAUqV4"

  putStrLn $ "Private key :" <> show myPrivKey1
  putStrLn $ "Public key :" <> show myPubKey1

  void $ runWeb3' localHttpProvider $ do
    -- liftIO $ putStrLn "Get Account Info"
    -- maybeAccountInfo <- getAccountInfo myPubKey1
    -- liftIO $ print maybeAccountInfo

    -- liftIO $ putStrLn "Get Block Height"
    -- height <- getBlockHeight
    -- liftIO $ print height

    -- liftIO $ putStrLn "Get Block Commitment"
    -- bc <- getBlockCommitment 370690637
    -- liftIO $ print bc

    -- liftIO $ putStrLn "Get Block Production"
    -- bp <- getBlockProduction
    -- liftIO $ print bp

    -- liftIO $ putStrLn "Get Blocks"
    -- lb <- getBlocks 370656000 (Just 370656010)
    -- liftIO $ print lb
    -- liftIO $ putStrLn "Get Blocks With Limit"
    -- lbl <- getBlocksWithLimit 370656000 11
    -- liftIO $ print lbl
    -- liftIO $ putStrLn "Get Blocks POSIX Time"
    -- bt <- getBlockTime 370656000
    -- liftIO $ print bt
    -- liftIO $ putStrLn "Get Cluster Nodes"
    -- cn <- getClusterNodes
    -- liftIO $ print cn
    -- liftIO $ putStrLn "Get Epoch Info"
    -- ei <- getEpochInfo
    -- liftIO $ print ei
    -- liftIO $ putStrLn "Get Epoch Schedule"
    -- es <- getEpochSchedule
    -- liftIO $ print es
    -- liftIO $ putStrLn "Get Fee For Message"
    -- fee <- getFeeForMessage "AQABAgIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEBAQAA"
    -- liftIO $ print fee
    -- let sampleBlockHash = unsafeBlockHash "J2Ce6VFKAffo4UxaZHdLexEYgHaDXdMbPdTinmU74b5N"
    -- let sampleMessage = fromRight mempty $ newMessageToBase64String sampleBlockHash [SystemProgram.transfer myPubKey1 myPubKey2 1_000_000_000]
    -- fee2 <- getFeeForMessage sampleMessage
    -- liftIO $ print fee2

    -- liftIO $ putStrLn "Get First Available Block"
    -- fab <- getFirstAvailableBlock
    -- liftIO $ print fab

    -- liftIO $ putStrLn "Get Genesis hash"
    -- gh <- getGenesisHash
    -- liftIO $ print gh

    -- liftIO $ putStrLn "Get Health"
    -- h <- getHealth
    -- liftIO $ print h

    -- liftIO $ putStrLn "Get Heighest Snapshot Slot"
    -- hss <- getHighestSnapshotSlot
    -- liftIO $ print hss

    -- liftIO $ putStrLn "Get Identity"
    -- identity <- getIdentity
    -- liftIO $ print identity

    -- liftIO $ putStrLn "Get Inflation Governor"
    -- igov <- getInflationGovernor
    -- liftIO $ print igov

    -- liftIO $ putStrLn "Get Inflation Rate"
    -- inrate <- getInflationRate
    -- liftIO $ print inrate

    -- liftIO $ putStrLn "Get Inflation Reward"
    -- infrw <- getInflationReward [show myPubKey1]
    -- liftIO $ print infrw

    -- liftIO $ putStrLn "Get Largest Accounts"
    -- laccs <- getLargestAccounts
    -- liftIO $ print laccs

    -- liftIO $ putStrLn "Get Latest Blockhash"
    -- lbh <- geTheLatestBlockhash
    -- liftIO $ print lbh

    -- liftIO $ putStrLn "Get Leader Schedule"
    -- ldsc <- getLeaderSchedule Nothing
    -- liftIO $ print ldsc

    -- liftIO $ putStrLn "Get Max Retransmit Slot"
    -- mrs <- getMaxRetransmitSlot
    -- liftIO $ print mrs

    -- liftIO $ putStrLn "Get Max Shred Insert Slot"
    -- mis <- getMaxShredInsertSlot
    -- liftIO $ print mis

    -- liftIO $ putStrLn "Get Minimum Balance For Rent Exemption"
    -- mb <- getMinimumBalanceForRentExemption 50
    -- liftIO $ print mb

    -- liftIO $ putStrLn "Get Multiple Accounts"
    -- accs <- getMultipleAccounts [myPubKey1, myPubKey2]
    -- liftIO $ print accs

    -- liftIO $ putStrLn "Get Program Accounts"
    -- paccs <- getProgramAccounts (unsafeSolanaPublicKey "11111111111111111111111111111111")
    -- liftIO $ print paccs

    -- liftIO $ putStrLn "Get Recent Performance Samples"
    -- perfsamples <- getRecentPerformanceSamples Nothing
    -- liftIO $ print perfsamples

    -- liftIO $ putStrLn "Get Prioritization Fee"
    -- priofee1 <- getRecentPrioritizationFees Nothing
    -- liftIO $ print priofee1

    -- liftIO $ putStrLn "Get Prioritization Fee"
    -- priofee2 <- getRecentPrioritizationFees (Just [])
    -- liftIO $ print priofee2

    -- liftIO $ putStrLn "Get Signatures For Address"
    -- sigs <- getSignaturesForAddress (unsafeSolanaPublicKey "Vote111111111111111111111111111111111111111")
    -- liftIO $ print sigs

    return ()
