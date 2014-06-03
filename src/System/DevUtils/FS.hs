module System.DevUtils.FS (
 getFilesAndDirectories,
 walkFilesAndDirectories,
 mustFileExist,
 mustNotFileExist,
 mustDirectoryExist,
 mustNotDirectoryExist
) where

import System.DevUtils.Control.Monad (concatMapM)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (assert)
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


mustFileExist :: FilePath -> IO Bool
mustFileExist = fileExist True


mustNotFileExist :: FilePath -> IO Bool
mustNotFileExist = fileExist (not True)


fileExist :: Bool -> FilePath -> IO Bool
fileExist t f = do
 r <- doesFileExist f
 assert (r == t) $ do return True


mustDirectoryExist :: FilePath -> IO Bool
mustDirectoryExist = directoryExist True


mustNotDirectoryExist :: FilePath -> IO Bool
mustNotDirectoryExist = directoryExist (not True)


directoryExist :: Bool -> FilePath -> IO Bool
directoryExist t f = do
 r <- doesDirectoryExist f
 assert (r == t) $ do return True
