-- 'sequentialTestGroup' (used below) is deprecated as of tasty-1.5.4 in
-- favor of 'dependentTestGroup', which has the same signature but only
-- exists since 1.5.4 -- newer than this project's declared @tasty ^>=1.5@
-- lower bound. Using the deprecated name keeps the suite building against
-- the whole declared range instead of silently requiring 1.5.4+; suppressed
-- here rather than left as a stray warning.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main (main) where

import Control.Exception (SomeException, try)
import Network.Solana.RPC.HTTP.Chain (getHealth)
import Network.Web3.Provider (Provider (HttpProvider), runWeb3')
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout (timeout)
import Test.Integration.Alt qualified as Alt
import Test.Integration.Block qualified as Block
import Test.Integration.Nonce qualified as Nonce
import Test.Integration.PriorityFee qualified as PriorityFee
import Test.Integration.Setup (rpcUrl)
import Test.Integration.Token qualified as Token
import Test.Integration.Transfer qualified as Transfer
import Test.Integration.WebSocket qualified as WebSocket
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
          putStrLn ("integration-tests: no validator at " <> url <> " - skipping (start solana-test-validator, or set SOLANA_INTEGRATION=1 to require it)")
          exitSuccess

-- | The suite has no @-threaded@/@-N@ RTS options, so 'testGroup' already
-- runs its children one at a time today; 'sequentialTestGroup' makes that a
-- load-bearing property of the test tree itself rather than an accident of
-- the cabal stanza, so the groups below (each hitting the same local
-- validator with fresh airdrops) can't start running concurrently if
-- @-threaded@ is ever added. 'AllFinish' keeps every group's results visible
-- even if an earlier one fails, matching today's behaviour.
--
-- One consequence of the dependency chain 'sequentialTestGroup' builds:
-- filtering to a single later group (e.g. @Alt@) with tasty's @-p@ pattern
-- option also force-includes every earlier group in that run (tasty's
-- pattern filter keeps whatever a dependency-chained test depends on), so
-- @-p@ no longer isolates just the matched test the way it would under a
-- plain 'testGroup'.
integrationTests :: TestTree
integrationTests = sequentialTestGroup "integration" AllFinish [Transfer.tests, Token.tests, Nonce.tests, PriorityFee.tests, Alt.tests, WebSocket.tests, Block.tests]
