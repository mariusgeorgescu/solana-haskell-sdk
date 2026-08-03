-- | Live PubSub coverage: confirm a real transaction by subscribing to its
-- signature over a WebSocket instead of polling for its status.
--
-- This is also the worked example of wiring
-- 'Network.Solana.RPC.WebSocket.WsTransport' to the @websockets@ package,
-- which the SDK deliberately does not depend on (see that module's header).
module Test.Integration.WebSocket (tests) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Network.Solana.Core.Crypto (SolanaPublicKey, SolanaSignature, createSolanaKeyPair)
import Network.Solana.Core.Message (newTransactionIntentWithPayer)
import Network.Solana.NativePrograms.SystemProgram qualified as SystemProgram
import Network.Solana.RPC.HTTP.Account (getBalance)
import Network.Solana.RPC.HTTP.Block (getTheLatestBlockhash)
import Network.Solana.RPC.WebSocket
import Network.Web3.Provider (Web3)
import Network.WebSockets qualified as WS
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "WebSocket"
    [testCase "signature subscription pushes the confirmation" wsConfirmFlow]

-- | Submits a transfer, then waits for its confirmation over the PubSub
-- endpoint rather than polling.
--
-- The transaction is sent first and subscribed to immediately afterwards, at
-- @finalized@ commitment: finalization takes on the order of ten seconds even
-- on a local validator, so the subscription is established far in advance of
-- the notification it is waiting for.
wsConfirmFlow :: IO ()
wsConfirmFlow = do
  (recipient, sig) <- run submitTransfer
  (host, port) <- wsEndpoint
  result <-
    WS.runClient host port "/" $ \conn ->
      let transport = WsTransport (WS.sendTextData conn) (WS.receiveData conn)
       in awaitSignature transport (RequestId 1) (Just "finalized") 90 sig
  slot <- either assertFailure' pure result
  assertBool "confirmation reports a real slot" (slot > 0)
  balance <- run (getBalance recipient)
  balance @?= 400_000_000

-- | Airdrops a payer and sends it a transfer, returning the recipient and the
-- submitted signature without waiting for confirmation -- that is what the
-- subscription is for.
submitTransfer :: Web3 (SolanaPublicKey, SolanaSignature)
submitTransfer = do
  (payerPk, payerSk) <- fundedKeypair 2_000_000_000
  recipient <- fst <$> liftIO createSolanaKeyPair
  bh <- getTheLatestBlockhash
  tx <-
    either
      (liftIO . throwIO)
      pure
      (newTransactionIntentWithPayer payerPk [payerSk] [SystemProgram.transfer payerPk recipient 400_000_000] bh)
  sig <- sendConfirmedPreflight tx
  pure (recipient, sig)

assertFailure' :: String -> IO a
assertFailure' = assertFailure
