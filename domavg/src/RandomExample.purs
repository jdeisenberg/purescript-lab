module RandomExample where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random (random)
import Control.Monad.Eff.Console (logShow)

printRandom = do
  n <- random
  logShow n
