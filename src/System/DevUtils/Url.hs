module System.DevUtils.Url (
 Url(..)
) where

import System.DevUtils.Redis
import System.DevUtils.Ssh
import System.DevUtils.Auth
import System.DevUtils.Connection

data Url =
 UrlRedis Redis
 | UrlSsh Ssh
 | UrlAuth Auth
 | UrlConnection Connection
 | UrlNone
 deriving (Show, Read)
