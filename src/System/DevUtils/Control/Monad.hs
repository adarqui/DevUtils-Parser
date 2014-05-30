module System.DevUtils.Control.Monad (
	concatMapM) where

import Control.Monad (liftM, mapM)

concatMapM f xs = liftM concat (mapM f xs)
