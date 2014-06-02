module System.DevUtils.File (
 File(..),
 defaultFile
) where

import qualified System.DevUtils.Connection as C
import qualified System.DevUtils.Session as Ses

data File = File {
 _path :: FilePath
} deriving (Show, Read)

defaultFile :: File
defaultFile = File {
  _path = "/tmp/poop"
 }
