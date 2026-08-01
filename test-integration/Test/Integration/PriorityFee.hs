-- | Live priority-fee coverage: a transfer prioritized with Compute Budget
-- instructions, and a smoke check of 'estimatePriorityFee' against the
-- resulting fee history.
module Test.Integration.PriorityFee (tests) where

import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.NativePrograms.ComputeBudget qualified as ComputeBudget
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.SolanaWeb3 (estimatePriorityFee)
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

-- Group order matters: 'priorityFeeTransfer' must run before
-- 'estimateSmoke' -- it creates the fee history 'estimateSmoke' relies on,
-- and tasty runs this group's cases sequentially.
tests :: TestTree
tests =
  testGroup
    "PriorityFee"
    [ testCase "prioritized transfer lands with the exact amount" (run priorityFeeTransfer),
      testCase "estimatePriorityFee smoke: returns without throwing" (run estimateSmoke)
    ]

-- | Sends a transfer prioritized with a compute unit limit and price, and
-- checks the recipient receives exactly the transferred amount.
priorityFeeTransfer :: Web3 ()
priorityFeeTransfer = do
  (payerPk, payerSk) <- fundedKeypair 2_000_000_000
  recipient <- fst <$> liftIO createSolanaKeyPair
  sig <-
    sendAndConfirm
      [payerSk]
      [ ComputeBudget.setComputeUnitLimit 20_000,
        ComputeBudget.setComputeUnitPrice 1_000,
        SystemProgram.transfer payerPk recipient 300_000_000
      ]
  confirmFinalized sig
  bal <- getBalance recipient
  liftIO (bal @?= 300_000_000)

-- | Runs AFTER 'priorityFeeTransfer' in the group: fee history exists.
-- Smoke check only -- the estimate is validator-dependent, so this asserts
-- 'estimatePriorityFee' returns without throwing rather than any value.
estimateSmoke :: Web3 ()
estimateSmoke = do
  pk <- fst <$> liftIO createSolanaKeyPair
  fee <- estimatePriorityFee [pk] 0.5
  liftIO (fee `seq` pure ())
