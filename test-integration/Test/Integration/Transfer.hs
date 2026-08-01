-- | Live SOL transfer coverage: a plain transfer from a freshly airdropped
-- payer, and a sponsored transfer where the fee payer and the transfer's
-- signer are different accounts.
module Test.Integration.Transfer (tests) where

import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.Core.Message (newTransactionIntentWithPayer)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Chain (getVersion)
import Network.Solana.RPC.HTTP.Transaction (sendTransaction)
import Network.Solana.SolanaWeb3 (newTransaction)
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Transfer"
    [ testCase "getVersion answers" (run (void getVersion)),
      testCase "airdropped SOL transfer lands with the exact amount" (run basicTransfer),
      testCase "sponsored transfer: sponsor pays the fee, signer order is free" (run sponsoredTransfer)
    ]

-- | Airdrops a payer, sends it through 'newTransaction' (which builds its
-- own blockhash and derives the fee payer from signer order), and checks the
-- recipient receives exactly the transferred amount. Live-covers
-- 'newTransaction' with every dependency (airdrop, blockhash, confirmation)
-- finalized.
basicTransfer :: Web3 ()
basicTransfer = do
  (payerPk, payerSk) <- fundedKeypair 2_000_000_000
  recipient <- fst <$> liftIO createSolanaKeyPair
  sig <- newTransaction [payerSk] [SystemProgram.transfer payerPk recipient 1_000_000_000]
  confirmFinalized sig
  bal <- getBalance recipient
  liftIO (bal @?= 1_000_000_000)

-- | A sponsor pays the fee for a transfer sent by another funded account, with
-- the private keys deliberately passed in the wrong order to
-- 'newTransactionIntentWithPayer' -- its auto-ordering must still produce a
-- valid transaction. Checks that the recipient and sender balances reflect
-- only the transfer amount (the sender pays no fee) and that the sponsor's
-- balance dropped by more than the transfer (it paid the fee).
sponsoredTransfer :: Web3 ()
sponsoredTransfer = do
  pairs <- fundedKeypairs [2_000_000_000, 1_000_000_000]
  ((sponsorPk, sponsorSk), (senderPk, senderSk)) <- case pairs of
    [a, b] -> pure (a, b)
    _ -> liftIO (throwIO (userError "expected two keypairs"))
  recipient <- fst <$> liftIO createSolanaKeyPair
  bh <- getTheLatestBlockhash
  -- signers deliberately in the WRONG order: auto-ordering must fix it
  tx <-
    either
      (liftIO . throwIO)
      pure
      (newTransactionIntentWithPayer sponsorPk [senderSk, sponsorSk] [SystemProgram.transfer senderPk recipient 500_000_000] bh)
  sig <- sendTransaction tx
  confirmFinalized sig
  recipientBal <- getBalance recipient
  liftIO (recipientBal @?= 500_000_000)
  senderBal <- getBalance senderPk
  liftIO (senderBal @?= 500_000_000) -- sender paid NO fee: exactly the transfer left
  sponsorBal <- getBalance sponsorPk
  liftIO (assertBool "sponsor paid the fee" (sponsorBal < 2_000_000_000))
