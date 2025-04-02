{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_solana_haskell_sdk (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "solana_haskell_sdk"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Library for interacting with Solana blockchain."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
