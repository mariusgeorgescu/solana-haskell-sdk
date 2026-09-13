{-# LANGUAGE OverloadedStrings #-}

-- | Hand-written response shapes for "Network.Solana.RPC.HTTP.Types" that a
-- local validator never produces (older nodes and RPC proxies do), so they
-- cannot live in the recorded-fixture suite "Test.RPC.Parsers".
module Test.RPC.Types (tests) where

import Data.Aeson (eitherDecode)
import Network.Solana.RPC.HTTP.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "RPC types"
    [ testCase "Context without apiVersion parses" $
        case eitherDecode "{\"slot\":41}" :: Either String Context of
          Left err -> assertFailure err
          Right ctx -> do
            apiVersion ctx @?= Nothing
            contextSlot ctx @?= 41,
      testCase "Context with apiVersion null parses as Nothing" $
        case eitherDecode "{\"apiVersion\":null,\"slot\":41}" :: Either String Context of
          Left err -> assertFailure err
          Right ctx -> apiVersion ctx @?= Nothing,
      testCase "Context keeps apiVersion when present" $
        case eitherDecode "{\"apiVersion\":\"2.1.16\",\"slot\":41}" :: Either String Context of
          Left err -> assertFailure err
          Right ctx -> apiVersion ctx @?= Just "2.1.16",
      testCase "RPCResponse without context.apiVersion still yields its value" $
        case eitherDecode "{\"context\":{\"slot\":41},\"value\":7}" :: Either String (RPCResponse Int) of
          Left err -> assertFailure err
          Right r -> value r @?= 7
    ]
