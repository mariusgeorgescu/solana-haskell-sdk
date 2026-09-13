-- | Live durable-nonce coverage: create and initialize a nonce account, use
-- it via 'newNonceTransaction' to send a transfer, and check the durable
-- nonce advances and the transfer lands -- both after finalization and, as
-- the helper promises, immediately after a merely-confirmed create or
-- advance.
module Test.Integration.Nonce (tests) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Account (AccountInfo (dataField))
import Network.Solana.Core.Block (BlockHash)
import Network.Solana.Core.Crypto (SolanaPublicKey, createSolanaKeyPair)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getAccountInfo, getAccountInfo', getBalance)
import Network.Solana.RPC.HTTP.Tokenomics (getMinimumBalanceForRentExemption)
import Network.Solana.RPC.HTTP.Types (defaultConfigObject, value)
import Network.Solana.SolanaWeb3 (accountDataBytes, cfgNonceAccountConfirmed, getNonceAccount, newNonceTransaction)
import Network.Web3.Provider (Web3)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Nonce"
    [ testCase "durable-nonce flow: create, use via newNonceTransaction, nonce advances" (run nonceFlow),
      testCase "newNonceTransaction works immediately after a confirmed create and a confirmed advance" (run nonceImmediateUse)
    ]

-- | Creates and initializes a nonce account (payer as authority), checks its
-- decoded state before use, sends a transfer through 'newNonceTransaction',
-- and checks the durable nonce advanced and the transfer landed with the
-- exact amount.
--
-- 'getNonceAccount' reads at the node's default @finalized@ commitment, so
-- the create transaction is finalized before its state is read;
-- 'newNonceTransaction' itself needs only @confirmed@ state (see
-- 'nonceImmediateUse').
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
  confirmFinalized createSig -- getNonceAccount reads the finalized bank
  -- The node's default encoding (a bare base58 string) and the explicit
  -- base64 pair must decode to the same 80 bytes.
  raw <- value <$> getAccountInfo' noncePk defaultConfigObject
  b64 <- getAccountInfo noncePk
  liftIO (fmap dataField raw @?= fmap dataField b64)
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

-- | The helper's contract: usable as soon as the nonce account's creation or
-- last advance is merely @confirmed@, with no finalization wait. Sends two
-- transfers back to back, the second within the finalization window of the
-- first one's nonce advance.
nonceImmediateUse :: Web3 ()
nonceImmediateUse = do
  (payerPk, payerSk) <- fundedKeypair 2_000_000_000
  (noncePk, nonceSk) <- liftIO createSolanaKeyPair
  rent <- getMinimumBalanceForRentExemption 80
  _ <-
    sendAndConfirm
      [payerSk, nonceSk]
      [ SystemProgram.createAccount payerPk noncePk rent 80 SystemProgram.systemProgramId,
        SystemProgram.initializeNonceAccount noncePk payerPk
      ]
  nonce0 <- durableNonceAtConfirmed noncePk
  recipient <- fst <$> liftIO createSolanaKeyPair
  sig1 <- newNonceTransaction payerPk [payerSk] noncePk [SystemProgram.transfer payerPk recipient 100_000_000]
  confirmOrFail "first nonce transaction" sig1
  nonce1 <- durableNonceAtConfirmed noncePk
  liftIO (assertBool "durable nonce advanced after the first use" (nonce1 /= nonce0))
  sig2 <- newNonceTransaction payerPk [payerSk] noncePk [SystemProgram.transfer payerPk recipient 100_000_000]
  confirmFinalized sig2
  afterAcc <- requireJust "nonce account (after)" =<< getNonceAccount noncePk
  case afterAcc of
    SystemProgram.NonceInitialized _ dn _ -> liftIO (assertBool "durable nonce advanced after the second use" (dn /= nonce1))
    _ -> liftIO (throwIO (userError "nonce account no longer initialized"))
  bal <- getBalance recipient
  liftIO (bal @?= 200_000_000)

-- | The nonce account's current durable nonce read at @confirmed@
-- commitment -- the same read 'newNonceTransaction' performs.
-- 'getNonceAccount' reads at the node's default @finalized@ commitment and
-- would still report the pre-advance value here.
durableNonceAtConfirmed :: SolanaPublicKey -> Web3 BlockHash
durableNonceAtConfirmed noncePk = do
  acc <- requireJust "nonce account (confirmed)" . value =<< getAccountInfo' noncePk cfgNonceAccountConfirmed
  state <- either (liftIO . throwIO . userError) pure (accountDataBytes (dataField acc) >>= SystemProgram.decodeNonceAccount)
  case state of
    SystemProgram.NonceInitialized _ dn _ -> pure dn
    _ -> liftIO (throwIO (userError "nonce account not initialized"))
