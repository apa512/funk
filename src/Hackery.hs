module Hackery
  ( compiledEnv
  ) where

import Language.Haskell.TH
import System.Environment

compiledEnv :: String -> Q Exp
compiledEnv s = runIO (getEnv s) >>= pure . LitE . stringL
