{-# LANGUAGE OverloadedStrings #-}

-- | Pure checks on the "Network.Solana.SolanaWeb3" helpers' configuration
-- values (the helpers themselves need a node; see the integration suite).
module Test.SolanaWeb3 (tests) where

import Data.Aeson (Value (String), object, toJSON, (.=))
import Network.Solana.RPC.HTTP.Types (commitment, preflightCommitment)
import Network.Solana.SolanaWeb3 (cfgNonceAccountConfirmed, cfgNonceSendConfirmed)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "SolanaWeb3"
    [ testCase "newNonceTransaction preflights at the commitment it reads the nonce at" $
        preflightCommitment cfgNonceSendConfirmed @?= commitment cfgNonceAccountConfirmed,
      testCase "nonce commitment is confirmed (finalized/finalized would silently drop advanced-nonce sends)" $
        commitment cfgNonceAccountConfirmed @?= Just "confirmed",
      testCase "nonce send config wire form keeps preflight on and pins it" $
        toJSON cfgNonceSendConfirmed
          @?= object ["encoding" .= String "base64", "preflightCommitment" .= String "confirmed"]
    ]
