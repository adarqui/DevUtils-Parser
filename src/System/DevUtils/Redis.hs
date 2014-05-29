module System.DevUtils.Redis (
 Redis(..),
 defaultRedis
) where

import qualified System.DevUtils.Auth as A
import qualified System.DevUtils.Connection as C

import Data.Maybe

data Redis = Redis {
 _auth :: Maybe A.Auth,
 _con :: C.Connection
-- _db :: Integer,
-- _prefix :: Maybe String
} deriving (Show, Read)

defaultRedis :: Redis
defaultRedis = Redis { _auth = Nothing, _con = C.defaultConnection }
