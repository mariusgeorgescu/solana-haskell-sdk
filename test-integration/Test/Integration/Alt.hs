-- | Live address-lookup-table coverage: create a table, extend it with
-- addresses, decode it back, and send a v0 transaction that resolves its
-- recipient through the table (the fee payer and the System Program id stay
-- static keys -- signers and program ids are never eligible for a lookup).
module Test.Integration.Alt (tests) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.Core.VersionedMessage (altAddresses, newV0TransactionIntent, newV0TransactionIntentWithPayer)
import Network.Solana.NativePrograms.AddressLookupTable (createLookupTable, extendLookupTable)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Ledger (getSlot)
import Network.Solana.RPC.HTTP.Transaction (sendTransaction)
import Network.Solana.SolanaWeb3 (getLookupTable)
import Network.Solana.SplPrograms.Memo qualified as Memo
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Alt"
    [ testCase "on-chain ALT: create, extend, decode, v0 transfer through the table" (run altV0Flow),
      testCase "v0 memo-only transaction via explicit fee payer lands without a lookup table" (run v0MemoOnly)
    ]

-- | Creates a lookup table at the current finalized slot (guaranteed present
-- in the SlotHashes sysvar for the on-chain create check), extends it with a
-- recipient and a spare address, waits for the extension to finalize, checks
-- the decoded table holds the extended addresses, then sends a v0
-- transaction that resolves the recipient through the table and checks the
-- transfer lands with the exact amount.
--
-- The v0 send goes through 'sendConfirmedPreflight' rather than a
-- @finalized@-commitment send: an address added to a lookup table in slot
-- @S@ (the extend's landing slot, exposed on-chain as
-- @last_extended_slot@) only resolves in slots strictly greater than @S@.
-- 'confirmFinalized' guarantees the rooted bank has reached @S@, not passed
-- it, so a @finalized@-commitment preflight can simulate against a bank
-- sitting exactly at @S@ -- where the table's active-address count is still
-- zero (it was empty before this extend) -- and reject the send with
-- \"Transaction address table lookup uses an invalid index\", even though
-- the extend is itself finalized and a fresh 'getLookupTable' read shows the
-- addresses. See 'sendConfirmedPreflight' for the full rationale.
altV0Flow :: Web3 ()
altV0Flow = do
  (payerPk, payerSk) <- fundedKeypair 3_000_000_000
  recipient <- fst <$> liftIO createSolanaKeyPair
  extra <- fst <$> liftIO createSolanaKeyPair
  slot <- getSlot
  let (createIx, tablePk) = createLookupTable payerPk payerPk slot
  _ <- sendAndConfirm [payerSk] [createIx]
  extendSig <- sendAndConfirm [payerSk] [extendLookupTable tablePk payerPk (Just payerPk) [recipient, extra]]
  confirmFinalized extendSig
  table <- requireJust "lookup table" =<< getLookupTable tablePk
  liftIO (assertBool "extended addresses present" (recipient `elem` altAddresses table))
  bh <- getTheLatestBlockhash
  tx <- either (liftIO . throwIO) pure (newV0TransactionIntent [payerSk] [SystemProgram.transfer payerPk recipient 200_000_000] [table] bh)
  sig <- sendConfirmedPreflight tx
  confirmFinalized sig
  bal <- getBalance recipient
  liftIO (bal @?= 200_000_000)

-- | The v0 counterpart of the legacy memo-only case: the memo's only signer
-- is read-only, so only an explicitly named fee payer yields a valid header.
v0MemoOnly :: Web3 ()
v0MemoOnly = do
  (payerPk, payerSk) <- fundedKeypair 1_000_000_000
  bh <- getTheLatestBlockhash
  tx <-
    either
      (liftIO . throwIO)
      pure
      (newV0TransactionIntentWithPayer payerPk [payerSk] [Memo.buildMemo "v0" [payerPk]] [] bh)
  sig <- sendTransaction tx
  confirmFinalized sig
