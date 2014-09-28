module System.DevUtils.Parser.Cmd (
 Cmd(..)
) where

import System.DevUtils.Base.Url.Redis
import System.DevUtils.Base.Url.MySQL
import System.DevUtils.Base.Url.Ssh
import System.DevUtils.Base.Url.ZMQ
import System.DevUtils.Base.Url.File
import System.DevUtils.Base.Url.Auth
import System.DevUtils.Base.Url.Connection
import System.DevUtils.Base.Url.Session
import System.DevUtils.Base.Url.Field

data Cmd =
 UrlRedis Redis
 | UrlResque Redis
 | UrlMySQL MySQL
 | UrlSsh Ssh
 | UrlZMQ ZMQ
 | UrlFile File
 | UrlAuth Auth
 | UrlConnection Connection
 | UrlSession Session
 | UrlChan
 | UrlMVar
 | UrlIORef
 | SepFields Fields
 | CmdNone
 deriving (Show, Read)
