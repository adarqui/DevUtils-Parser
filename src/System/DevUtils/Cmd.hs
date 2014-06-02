module System.DevUtils.Cmd (
 Cmd(..)
) where

import System.DevUtils.Redis
import System.DevUtils.Ssh
import System.DevUtils.Auth
import System.DevUtils.Connection
import System.DevUtils.Session
import System.DevUtils.Field

data Cmd =
 UrlRedis Redis
 | UrlSsh Ssh
 | UrlAuth Auth
 | UrlConnection Connection
 | UrlSession Session
 | SepFields Fields
 | CmdNone
 deriving (Show, Read)
