-- | Live SOL transfer coverage: a plain transfer from a freshly airdropped
-- payer, a sponsored transfer where the fee payer and the transfer's signer
-- are different accounts, and the fee-payer contract of the public
-- 'newTransaction' helper (first key pays, read-only-signer-only
-- instructions land, empty signer list rejected).
module Test.Integration.Transfer (tests) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.Core.Message (newTransactionIntentWithPayer)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Chain (getVersion)
import Network.Solana.RPC.HTTP.Transaction (sendTransaction)
import Network.Solana.SolanaWeb3 (newTransaction)
import Network.Solana.SplPrograms.Memo qualified as Memo
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
      testCase "sponsored transfer: sponsor pays the fee, signer order is free" (run sponsoredTransfer),
      testCase "newTransaction: memo-only instruction (read-only authority signer) lands" (run memoOnly),
      testCase "newTransaction: empty signer list is rejected before any RPC call" noSigners,
      testCase "newTransaction: first key pays even when unreferenced, signer order is free" (run firstKeyPays)
    ]

-- | Airdrops a payer, sends it through 'newTransaction' (which builds its
-- own blockhash and names its first key as the fee payer), and checks the
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

-- | A memo instruction's only account is its signer as a /read-only/ signer,
-- so a fee payer derived from the instructions alone would be read-only and
-- the node would reject the transaction at sanitize time. 'newTransaction'
-- must name its first key as the (writable) fee payer instead.
memoOnly :: Web3 ()
memoOnly = do
  (payerPk, payerSk) <- fundedKeypair 1_000_000_000
  sig <- newTransaction [payerSk] [Memo.buildMemo "solana-haskell-sdk" [payerPk]]
  confirmFinalized sig

-- | With no signing key there is no fee payer; 'newTransaction' must say so
-- before touching the network rather than submit a signature-less
-- transaction for the node to reject.
noSigners :: Assertion
noSigners = do
  pk <- fst <$> createSolanaKeyPair
  recipient <- fst <$> createSolanaKeyPair
  r <- try @SomeException (run (newTransaction [] [SystemProgram.transfer pk recipient 1]))
  case r of
    Left e -> assertBool ("expected a \"no signers\" error, got: " <> show e) ("no signers" `isInfixOf` show e)
    Right sig -> assertFailure ("sent a transaction with no signers: " <> show sig)

-- | The first key given to 'newTransaction' pays the fee even if no
-- instruction references it (sponsored fees), and the remaining keys may be
-- passed in any order.
firstKeyPays :: Web3 ()
firstKeyPays = do
  pairs <- fundedKeypairs [2_000_000_000, 1_000_000_000]
  ((sponsorPk, sponsorSk), (senderPk, senderSk)) <- case pairs of
    [a, b] -> pure (a, b)
    _ -> liftIO (throwIO (userError "expected two keypairs"))
  recipient <- fst <$> liftIO createSolanaKeyPair
  sig <- newTransaction [sponsorSk, senderSk] [SystemProgram.transfer senderPk recipient 500_000_000]
  confirmFinalized sig
  recipientBal <- getBalance recipient
  liftIO (recipientBal @?= 500_000_000)
  senderBal <- getBalance senderPk
  liftIO (senderBal @?= 500_000_000) -- sender paid NO fee: exactly the transfer left
  sponsorBal <- getBalance sponsorPk
  liftIO (assertBool "sponsor paid the fee" (sponsorBal < 2_000_000_000))
