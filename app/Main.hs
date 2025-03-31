{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad.IO.Class (MonadIO (..))
import Core.Block (unsafeBlockHash)
import Core.Crypto
import Core.Message (newMessage, newMessageToBase64String, newTransactionIntent)
import Data.Aeson (ToJSON (toJSON))
import Data.Either (fromRight)
import Data.Functor (void)
import NativePrograms.SystemProgram qualified as SystemProgram
import Network.Web3.Provider
import RPC.HTTP.Account
import RPC.HTTP.Block
import RPC.HTTP.Chain
import RPC.HTTP.Token
import RPC.HTTP.Transaction
import RPC.Providers

testTx :: IO ()
testTx = void $ runWeb3' localHttpProvider $ do
  (myPubKey1, myPrivKey1) <- liftIO createSolanaKeyPair
  (myPubKey2, myPrivKey2) <- liftIO createSolanaKeyPair
  liftIO $ print $ "Pubkey 1: " <> show myPubKey1
  liftIO $ print $ "Pubkey 2: " <> show myPubKey2

  liftIO $ putStrLn $ "Requesting airdrop for" <> show myPubKey1
  txId1 <- requestAirdrop myPubKey1 10_000_000_000
  liftIO $ putStrLn $ "Requesting airdrop for" <> show myPubKey2
  txId2 <- requestAirdrop myPubKey2 1_000_000_000
  liftIO $ putStrLn "Waiting for 15 seconds .."
  liftIO $ threadDelay (15 * 1000000)

  balance1 <- getBalance myPubKey1
  balance2 <- getBalance myPubKey2
  liftIO $ putStrLn $ "Balance for " <> show myPubKey1
  liftIO $ print balance1
  liftIO $ putStrLn $ "Balance for " <> show myPubKey2
  liftIO $ print balance2

  liftIO $ putStrLn ("Getting latest blockhash ..." :: String)
  bh <- getTheLatestBlockhash
  liftIO $ putStrLn $ "Latest Blockhash: " <> show bh

  let newTx =
        newTransactionIntent
          [myPrivKey1]
          [ SystemProgram.transfer myPubKey1 myPubKey2 1_000_000_000
          ]

  signedTx <- liftIO $ either throw return (newTx bh)
  liftIO $ putStrLn ("Signed tx: " <> signedTx)

  liftIO $ putStrLn "Simulate tx"
  result <- simulateTransaction signedTx
  liftIO $ print result

  liftIO $ putStrLn "Submiting tx"
  tId <- sendTransaction signedTx
  let txId = unsafeSigFromString tId
  liftIO $ putStrLn ("Transaction id: " <> show txId)
  liftIO $ putStrLn "Waiting for 15 seconds .."
  liftIO $ threadDelay (15 * 1000000)

  balance1 <- getBalance myPubKey1
  balance2 <- getBalance myPubKey2
  liftIO $ putStrLn $ "Balance for" <> show myPubKey1
  liftIO $ print balance1
  liftIO $ putStrLn $ "Balance for" <> show myPubKey2
  liftIO $ print balance2

main :: IO ()
main = do
  let myPrivKey1 = unsafeSolanaPrivateKeyRaw [22, 92, 4, 245, 212, 153, 201, 54, 28, 97, 151, 138, 173, 130, 57, 22, 83, 124, 200, 60, 185, 35, 153, 114, 83, 74, 208, 151, 167, 200, 251, 120, 61, 127, 0, 228, 106, 0, 194, 74, 250, 228, 47, 77, 167, 219, 246, 233, 132, 0, 104, 179, 87, 70, 129, 53, 241, 74, 191, 236, 104, 204, 2, 113]
  let myPubKey1 = toSolanaPublicKey myPrivKey1
  let myPubKey2 = unsafeSolanaPublicKey "A988FuUtUVk8jMUuVc1ccaoTA3VS9CB4dkEf9EUAUqV4"

  void $ runWeb3' localHttpProvider $ do
    currentSlot <- getSlot
    currentBlock <- getBlock currentSlot
    liftIO $ print currentBlock
    vac <- getVoteAccounts
    liftIO $ print vac

    balance1 <- getBalance myPubKey1
    balance2 <- getBalance myPubKey2
    liftIO $ putStrLn $ "Balance for " <> show myPubKey1
    liftIO $ print balance1
    liftIO $ putStrLn $ "Balance for " <> show myPubKey2
    liftIO $ print balance2

    accs <- getTokenAccountsByOwner myPubKey1 (Mint (unsafeSolanaPublicKey "CEk5uWHcwNxFGN9md54dpivZAzY3oYWtubCxJ3XJ518u"))
    liftIO $ print accs

    liftIO $ putStrLn "Get Account Info"
    maybeAccountInfo <- getAccountInfo myPubKey1
    liftIO $ print maybeAccountInfo

    liftIO $ putStrLn "Get Block Height"
    height <- getBlockHeight
    liftIO $ print height

    liftIO $ putStrLn "Get Block Commitment"
    bc <- getBlockCommitment 370690637
    liftIO $ print bc

    liftIO $ putStrLn "Get Block Production"
    bp <- getBlockProduction
    liftIO $ print bp

    liftIO $ putStrLn "Get Blocks"
    lb <- getBlocks 370656000 (Just 370656010)
    liftIO $ print lb
    liftIO $ putStrLn "Get Blocks With Limit"
    lbl <- getBlocksWithLimit 370656000 11
    liftIO $ print lbl
    liftIO $ putStrLn "Get Blocks POSIX Time"
    bt <- getBlockTime currentSlot
    liftIO $ print bt
    liftIO $ putStrLn "Get Cluster Nodes"
    cn <- getClusterNodes
    liftIO $ print cn
    liftIO $ putStrLn "Get Epoch Info"
    ei <- getEpochInfo
    liftIO $ print ei
    liftIO $ putStrLn "Get Epoch Schedule"
    es <- getEpochSchedule
    liftIO $ print es
    liftIO $ putStrLn "Get Fee For Message"
    fee <- getFeeForMessage "AQABAgIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEBAQAA"
    liftIO $ print fee
    let sampleBlockHash = unsafeBlockHash "J2Ce6VFKAffo4UxaZHdLexEYgHaDXdMbPdTinmU74b5N"
    let sampleMessage = fromRight mempty $ newMessageToBase64String sampleBlockHash [SystemProgram.transfer myPubKey1 myPubKey2 1_000_000_000]
    fee2 <- getFeeForMessage sampleMessage
    liftIO $ print fee2

    liftIO $ putStrLn "Get First Available Block"
    fab <- getFirstAvailableBlock
    liftIO $ print fab

    liftIO $ putStrLn "Get Genesis hash"
    gh <- getGenesisHash
    liftIO $ print gh

    liftIO $ putStrLn "Get Health"
    h <- getHealth
    liftIO $ print h

    liftIO $ putStrLn "Get Heighest Snapshot Slot"
    hss <- getHighestSnapshotSlot
    liftIO $ print hss

    liftIO $ putStrLn "Get Identity"
    identity <- getIdentity
    liftIO $ print identity

    liftIO $ putStrLn "Get Inflation Governor"
    igov <- getInflationGovernor
    liftIO $ print igov

    liftIO $ putStrLn "Get Inflation Rate"
    inrate <- getInflationRate
    liftIO $ print inrate

    -- liftIO $ putStrLn "Get Inflation Reward"
    -- infrw <- getInflationReward [ myPubKey1]
    -- liftIO $ print infrw

    liftIO $ putStrLn "Get Largest Accounts"
    laccs <- getLargestAccounts
    liftIO $ print laccs

    liftIO $ putStrLn "Get Latest Blockhash"
    lbh <- getTheLatestBlockhash
    liftIO $ print lbh

    liftIO $ putStrLn "Get Leader Schedule"
    ldsc <- getLeaderSchedule Nothing
    liftIO $ print ldsc

    liftIO $ putStrLn "Get Max Retransmit Slot"
    mrs <- getMaxRetransmitSlot
    liftIO $ print mrs

    liftIO $ putStrLn "Get Max Shred Insert Slot"
    mis <- getMaxShredInsertSlot
    liftIO $ print mis

    liftIO $ putStrLn "Get Minimum Balance For Rent Exemption"
    mb <- getMinimumBalanceForRentExemption 50
    liftIO $ print mb

    liftIO $ putStrLn "Get Multiple Accounts"
    accs <- getMultipleAccounts [myPubKey1, myPubKey2]
    liftIO $ print accs

    liftIO $ putStrLn "Get Program Accounts"
    paccs <- getProgramAccounts (unsafeSolanaPublicKey "11111111111111111111111111111111")
    liftIO $ print paccs

    liftIO $ putStrLn "Get Recent Performance Samples"
    perfsamples <- getRecentPerformanceSamples Nothing
    liftIO $ print perfsamples

    liftIO $ putStrLn "Get Prioritization Fee"
    priofee1 <- getRecentPrioritizationFees Nothing
    liftIO $ print priofee1

    liftIO $ putStrLn "Get Prioritization Fee"
    priofee2 <- getRecentPrioritizationFees (Just [])
    liftIO $ print priofee2

    liftIO $ putStrLn "Get Signatures For Address"
    sigs <- getSignaturesForAddress (unsafeSolanaPublicKey "Vote111111111111111111111111111111111111111")
    liftIO $ print sigs

    let txId1 = unsafeSigFromString "2bvMvz7c9qvYGoCXyRErTyC2BLGGNd3ayfPiP2j8aKwCUurnV2KXJ4WAt6geg7a9MZAQkAEHxb4kAbPKLCFgsXoc"
    let txId2 = unsafeSigFromString "3pywXYUaZgFbgs3yFKK6VZ8VSXMzwrDZzgnbsqcxMwetQRGvVvwDxARm2y8DUD7vCgwjPh3CmJCWtz65oiRbWANQ"
    liftIO $ print txId1
    liftIO $ print txId2

    liftIO $ putStrLn "Get Signature Statuses"
    sigsstats <- getSignatureStatuses [txId1, txId2]
    liftIO $ print sigsstats

    liftIO $ putStrLn "Get Slot"
    slot <- getSlot
    liftIO $ print slot

    liftIO $ putStrLn "Get Slot Leader"
    slotLeader <- getSlotLeader
    liftIO $ print slotLeader

    liftIO $ putStrLn "Get Slot Leaders"
    slotLeaders <- getSlotLeaders currentSlot 1
    liftIO $ print slotLeaders

    liftIO $ putStrLn "Get Stake Minimum Delegation"
    mindelegamnt <- getStakeMinimumDelegation
    liftIO $ print mindelegamnt

    liftIO $ putStrLn "Get Supply"
    supply <- getSupply
    liftIO $ print supply

    liftIO $ putStrLn "Get Token Account Balance"
    tab <- getTokenAccountBalance (unsafeSolanaPublicKey "5QzKcuk1nJQfYM88wQtD5vpZS1onA8Zpp2GaZft6wZ5H")
    liftIO $ print tab

    liftIO $ putStrLn "Get Token Largest Accounts"
    tlaccs <- getTokenLargestAccounts (unsafeSolanaPublicKey "CEk5uWHcwNxFGN9md54dpivZAzY3oYWtubCxJ3XJ518u")
    liftIO $ print tlaccs

    liftIO $ putStrLn "Get Token Supply"
    tsupply <- getTokenSupply (unsafeSolanaPublicKey "CEk5uWHcwNxFGN9md54dpivZAzY3oYWtubCxJ3XJ518u")
    liftIO $ print tsupply

    liftIO $ putStrLn "Get Transaction Count"
    txcount <- getTransactionCount
    liftIO $ print txcount

    liftIO $ putStrLn "Get Version"
    v <- getVersion
    liftIO $ print v

    liftIO $ putStrLn "Check Blockhash"
    bh <- getTheLatestBlockhash
    liftIO $ print (toJSON bh)
    ibhv <- isBlockhashValid bh
    liftIO $ print ibhv

    liftIO $ putStrLn "Minimum ledger slot"
    mls <- minimumLedgerSlot
    liftIO $ print mls
