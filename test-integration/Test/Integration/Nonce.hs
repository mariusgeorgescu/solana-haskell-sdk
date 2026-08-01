-- | Live durable-nonce coverage: create and initialize a nonce account, use
-- it via 'newNonceTransaction' to send a transfer, and check the durable
-- nonce advances and the transfer lands.
module Test.Integration.Nonce (tests) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (createSolanaKeyPair)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Tokenomics (getMinimumBalanceForRentExemption)
import Network.Solana.SolanaWeb3 (getNonceAccount, newNonceTransaction)
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Nonce"
    [testCase "durable-nonce flow: create, use via newNonceTransaction, nonce advances" (run nonceFlow)]

-- | Creates and initializes a nonce account (payer as authority), checks its
-- decoded state before use, sends a transfer through 'newNonceTransaction',
-- and checks the durable nonce advanced and the transfer landed with the
-- exact amount.
--
-- 'newNonceTransaction' reads the nonce account at @confirmed@ commitment
-- internally, but the node preflights its send against the @finalized@
-- bank, so the create transaction is finalized before it is called.
nonceFlow :: Web3 ()
nonceFlow = do
  (payerPk, payerSk) <- fundedKeypair 2_000_000_000
  (noncePk, nonceSk) <- liftIO createSolanaKeyPair
  rent <- getMinimumBalanceForRentExemption 80
  createSig <-
    sendAndConfirm
      [payerSk, nonceSk]
      [ SystemProgram.createAccount payerPk noncePk rent 80 SystemProgram.systemProgramId,
        SystemProgram.initializeNonceAccount noncePk payerPk
      ]
  confirmFinalized createSig -- newNonceTransaction preflights against the finalized bank
  before <- requireJust "nonce account (before)" =<< getNonceAccount noncePk
  nonceBefore <- case before of
    SystemProgram.NonceInitialized auth dn _ -> liftIO (auth @?= payerPk) >> pure dn
    _ -> liftIO (throwIO (userError "nonce account not initialized"))
  recipient <- fst <$> liftIO createSolanaKeyPair
  sig <- newNonceTransaction payerPk [payerSk] noncePk [SystemProgram.transfer payerPk recipient 100_000_000]
  confirmFinalized sig
  afterAcc <- requireJust "nonce account (after)" =<< getNonceAccount noncePk
  case afterAcc of
    SystemProgram.NonceInitialized _ dn _ -> liftIO (assertBool "durable nonce advanced" (dn /= nonceBefore))
    _ -> liftIO (throwIO (userError "nonce account no longer initialized"))
  bal <- getBalance recipient
  liftIO (bal @?= 100_000_000)
