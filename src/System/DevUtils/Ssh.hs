module System.DevUtils.Ssh (
 Ssh(..)
) where

import System.DevUtils.Auth
import System.DevUtils.Connection

data Ssh = Ssh {
 _auth :: Auth,
 _connection :: Connection,
 _cmd :: String
} deriving (Show, Read)
