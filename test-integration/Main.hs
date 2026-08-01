module Main (main) where

import Control.Exception (SomeException, try)
import Network.Solana.RPC.HTTP.Chain (getHealth)
import Network.Web3.Provider (Provider (HttpProvider), runWeb3')
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)
import Test.Integration.Setup (rpcUrl)
import Test.Integration.Transfer qualified as Transfer
import Test.Tasty
import Test.Tasty.HUnit

-- | Probes 'rpcUrl' for a healthy validator (3 s timeout) before running the
-- suite. No validator reachable: skip green, unless @SOLANA_INTEGRATION=1@ is
-- set, in which case an unreachable validator is a loud failure instead of a
-- silent skip.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  url <- rpcUrl
  probe <- timeout 3000000 (try @SomeException (runWeb3' (HttpProvider url) getHealth))
  case probe of
    Just (Right (Right "ok")) -> defaultMain integrationTests
    _ -> do
      force <- lookupEnv "SOLANA_INTEGRATION"
      if force == Just "1"
        then defaultMain (testCase "validator reachable" (assertFailure ("SOLANA_INTEGRATION=1 but no healthy validator at " <> url)))
        else do
          putStrLn ("integration-tests: no validator at " <> url <> " — skipping (start solana-test-validator, or set SOLANA_INTEGRATION=1 to require it)")
          exitSuccess

integrationTests :: TestTree
integrationTests = testGroup "integration" [Transfer.tests]
