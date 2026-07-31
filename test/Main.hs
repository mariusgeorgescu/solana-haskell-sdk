module Main (main) where

import Test.Core.Compact qualified
import Test.Core.Crypto qualified
import Test.Core.Instruction qualified
import Test.Core.Message qualified
import Test.Core.Pda qualified
import Test.NativePrograms.ComputeBudget qualified
import Test.NativePrograms.SystemProgram qualified
import Test.SplPrograms.Memo qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain
    ( testGroup
        "solana-haskell-sdk"
        [ Test.Core.Compact.tests,
          Test.Core.Crypto.tests,
          Test.Core.Instruction.tests,
          Test.Core.Message.tests,
          Test.Core.Pda.tests,
          Test.NativePrograms.ComputeBudget.tests,
          Test.NativePrograms.SystemProgram.tests,
          Test.SplPrograms.Memo.tests
        ]
    )
