module System.DevUtils.FS (
 getFilesAndDirectories
) where

import System.DevUtils.Control.Monad (concatMapM)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM)
import Data.List (filter, map)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

data Directory = Directory {
 _path :: FilePath,
 _files :: [FilePath],
 _directories :: [FilePath]
} deriving (Show, Read)

getFilesAndDirectories :: FilePath -> IO Directory
getFilesAndDirectories dir = do
 map (dir </>) . filter (`notElem` ["..", "."]) <$> getDirectoryContents dir >>= \c ->
  filterM doesDirectoryExist c >>= \d ->
   filterM doesFileExist c >>= \f ->
    return $ Directory {
     _path = dir,
     _directories = d,
     _files = f
    }

-- walkFilesAndDirectories (putStrLn . show) "/tmp/hi"
walkFilesAndDirectories :: (Directory -> IO a) -> FilePath -> IO [(Directory, a)]
walkFilesAndDirectories act dir = do
 getFilesAndDirectories dir >>= \d ->
  act d >>= \r ->
   (concatMapM (walkFilesAndDirectories act) $ (_directories d)) >>= \walked ->
    return $ [(d, r)] ++ walked
