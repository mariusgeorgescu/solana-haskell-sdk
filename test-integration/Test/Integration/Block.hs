-- | Live coverage of the block-retrieval contract: a slot holding no block
-- is answered by the node with a JSON-RPC error (raised as an exception the
-- transport does not turn into 'Left'), never with 'Nothing'.
module Test.Integration.Block (tests) where

import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Network.Solana.RPC.HTTP.Block (getBlock)
import Network.Solana.RPC.HTTP.Ledger (getSlot)
import Test.Integration.Setup
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Block"
    [ testCase "getBlock on a slot holding no block raises the node's -32004 error, not Nothing" $ do
        result <- try @SomeException (run (getSlot >>= \s -> getBlock (s + 100_000)))
        case result of
          Left e -> assertBool (show e) ("-32004" `isInfixOf` show e)
          Right b -> assertFailure ("expected a JSON-RPC error, got a result (Just: " <> show (isJust b) <> ")"),
      -- The current finalized slot: produced by the sole validator, and not yet
      -- pruned (old slots are cleaned up under the default ledger size limit).
      testCase "getBlock on a produced slot returns the block" $ do
        b <- run (getSlot >>= getBlock)
        assertBool "expected Just a block" (isJust b)
    ]
