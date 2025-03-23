module Core.TransactionBuilder where

import Control.Monad.State.Strict (StateT)
import Core.Transaction

data SolanaTxBuildingException = SolanaTxBuildingException

type SolanaTxBuilder = StateT Transaction (Either SolanaTxBuildingException)

