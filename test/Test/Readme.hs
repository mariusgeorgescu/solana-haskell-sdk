{-# LANGUAGE OverloadedStrings #-}

-- | Keeps the README's usage examples and the example executables under
-- @app/@ identical, so the text users copy is the text CI compiles.
module Test.Readme (tests) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Tasty
import Test.Tasty.HUnit

readUtf8 :: FilePath -> IO Text
readUtf8 path = TE.decodeUtf8 <$> BS.readFile path

-- | The fenced Haskell blocks of the README that are complete programs (they
-- open with a LANGUAGE pragma), in order of appearance.
programBlocks :: Text -> [Text]
programBlocks = go . T.lines
  where
    go [] = []
    go (l : ls)
      | l == "```haskell" =
          let (body, rest) = break (== "```") ls
              others = go (drop 1 rest)
           in if isProgram body then T.unlines body : others else others
      | otherwise = go ls
    isProgram (first : _) = "{-# LANGUAGE" `T.isPrefixOf` first
    isProgram [] = False

-- | The example executables, in the order their sources appear in the README.
examplePrograms :: [FilePath]
examplePrograms = ["app/Main.hs", "app/SplTransfer.hs", "app/PriorityFee.hs"]

tests :: TestTree
tests =
  testGroup
    "README"
    [ testCase "usage examples are byte-identical to the compiled example programs" $ do
        blocks <- programBlocks <$> readUtf8 "README.md"
        programs <- mapM readUtf8 examplePrograms
        blocks @?= programs
    ]
