module System.DevUtils.Session (
 Session(..),
 defaultSession
) where

import qualified System.DevUtils.Auth as A
import qualified System.DevUtils.Connection as C

data Session = Session {
 _auth :: Maybe A.Auth,
 _con :: C.Connection
} deriving (Show, Read)

defaultSession :: Session
defaultSession = Session { _auth = Nothing, _con = C.defaultConnection }
