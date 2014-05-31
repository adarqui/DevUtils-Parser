module System.DevUtils.Url (
 Url(..)
) where

import System.DevUtils.Redis
import System.DevUtils.Ssh
import System.DevUtils.Auth
import System.DevUtils.Connection
import System.DevUtils.Session

data Url =
 UrlRedis Redis
 | UrlSsh Ssh
 | UrlAuth Auth
 | UrlConnection Connection
 | UrlSession Session
 | UrlNone
 deriving (Show, Read)
