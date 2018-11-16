module TypeChecker where

import qualified Data.Map as Map
import Control.Monad

import AbsLua
import PrintLua
import ErrM
import Debug.Trace

data Env = Env [BlockEnv]
  deriving (Eq, Ord, Show, Read)

data BlockEnv = BlockEnv {
  funDefs :: Sigs,
  varDefs :: Context,
  blockTyp :: BlockTyp
}