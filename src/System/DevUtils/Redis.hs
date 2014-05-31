module System.DevUtils.Redis (
 Redis(..),
 defaultRedisSession
) where

import qualified System.DevUtils.Auth as A
import qualified System.DevUtils.Connection as C
import qualified System.DevUtils.Session as Ses

data Redis = Redis {
 _ses :: Ses.Session
-- _db :: Integer,
-- _prefix :: Maybe String
} deriving (Show, Read)

defaultRedisSession :: Ses.Session
defaultRedisSession = Ses.Session {
 Ses._auth = Nothing,
 Ses._con = C.Connection {
  C._dest = "localhost", C._port = 6379, C._type = C.TCP }
 }
