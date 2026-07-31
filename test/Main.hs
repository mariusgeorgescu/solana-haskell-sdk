module Main (main) where

import Test.Core.Compact qualified
import Test.Core.Crypto qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain
    ( testGroup
        "solana-haskell-sdk"
        [ Test.Core.Compact.tests,
          Test.Core.Crypto.tests
        ]
    )
