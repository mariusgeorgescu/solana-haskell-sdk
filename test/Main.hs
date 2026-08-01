module Main (main) where

import Test.Core.Account qualified
import Test.Core.Block qualified
import Test.Core.Compact qualified
import Test.Core.Crypto qualified
import Test.Core.Instruction qualified
import Test.Core.Message qualified
import Test.Core.Pda qualified
import Test.NativePrograms.BpfLoaderUpgradeable qualified
import Test.NativePrograms.ComputeBudget qualified
import Test.NativePrograms.Secp256k1 qualified
import Test.NativePrograms.Stake qualified
import Test.NativePrograms.SystemProgram qualified
import Test.NativePrograms.Vote qualified
import Test.SplPrograms.AssociatedTokenAccount qualified
import Test.SplPrograms.Memo qualified
import Test.SplPrograms.Token qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain
    ( testGroup
        "solana-haskell-sdk"
        [ Test.Core.Account.tests,
          Test.Core.Block.tests,
          Test.Core.Compact.tests,
          Test.Core.Crypto.tests,
          Test.Core.Instruction.tests,
          Test.Core.Message.tests,
          Test.Core.Pda.tests,
          Test.NativePrograms.BpfLoaderUpgradeable.tests,
          Test.NativePrograms.ComputeBudget.tests,
          Test.NativePrograms.Secp256k1.tests,
          Test.NativePrograms.Stake.tests,
          Test.NativePrograms.SystemProgram.tests,
          Test.NativePrograms.Vote.tests,
          Test.SplPrograms.AssociatedTokenAccount.tests,
          Test.SplPrograms.Memo.tests,
          Test.SplPrograms.Token.tests
        ]
    )
