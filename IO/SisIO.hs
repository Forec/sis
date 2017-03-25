module IO.SisIO where

import Control.Monad (unless)
import Control.DeepSeq (rnf)

import System.IO
import System.Directory (doesFileExist)

createFile :: FilePath -> IO ()
createFile path = do
  exist <- doesFileExist path
  Control.Monad.unless exist $ writeFile path ""

getFileReadHandle :: FilePath -> IO Handle
getFileReadHandle path = do
  createFile path
  openFile path ReadMode

getFileWriteHandle :: FilePath -> IO Handle
getFileWriteHandle path = do
  createFile path
  openFile path WriteMode

getContent :: FilePath -> IO String
getContent path = do
  handle' <- getFileReadHandle path
  contents <- hGetContents handle'
  rnf contents `seq` hClose handle'
  return contents