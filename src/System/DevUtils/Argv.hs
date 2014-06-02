module System.DevUtils.Argv (
 Argv(..)
) where

import System.DevUtils.Url

data Argv =
 ArgvUrl Url
 | ArgvNone
 deriving (Show, Read)
