-- | Live address-lookup-table coverage: create a table, extend it with
-- addresses, decode it back, and send a v0 transaction that resolves its fee
-- payer and recipient through the table instead of listing them in full.
module Test.Integration.Alt (tests) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.Core.VersionedMessage (altAddresses, newV0TransactionIntent)
import Network.Solana.NativePrograms.AddressLookupTable (createLookupTable, extendLookupTable)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.HTTP.Ledger (getSlot)
import Network.Solana.RPC.HTTP.Transaction (sendTransaction')
import Network.Solana.RPC.HTTP.Types (defaultConfigObject, encoding, preflightCommitment)
import Network.Solana.SolanaWeb3 (getLookupTable)
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Alt"
    [testCase "on-chain ALT: create, extend, decode, v0 transfer through the table" (run altV0Flow)]

-- | Creates a lookup table at the current finalized slot (guaranteed present
-- in the SlotHashes sysvar for the on-chain create check), extends it with a
-- recipient and a spare address, waits for the extension to finalize
-- (doubling as the table's warm-up -- it is usable once the current slot is
-- past its last-extension slot), checks the decoded table holds the
-- extended addresses, then sends a v0 transaction that resolves the payer
-- and recipient through the table and checks the transfer lands with the
-- exact amount.
--
-- The v0 send pins preflight at @confirmed@ rather than the node's default
-- @finalized@: observed empirically (5 back-to-back full-suite runs against
-- a freshly reset local validator) to matter for address-lookup-table
-- resolution specifically -- even once the extending transaction has
-- genuinely reached @finalized@ status (per 'confirmFinalized') and a fresh
-- 'getLookupTable' read confirms the extended addresses are present, a
-- @finalized@-commitment preflight can still intermittently reject the v0
-- send with \"Transaction address table lookup uses an invalid index\",
-- while @confirmed@ (much closer to the tip) did not reproduce the failure
-- across repeated back-to-back runs. Every other RPC call in this flow
-- keeps the node's @finalized@ default, per 'Test.Integration.Setup'.
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
  sig <- sendTransaction' tx (defaultConfigObject {encoding = Just "base64", preflightCommitment = Just "confirmed"})
  confirmFinalized sig
  bal <- getBalance recipient
  liftIO (bal @?= 200_000_000)
