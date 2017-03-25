module Core.SisCore where

import Data.Char

import Control.Exception
import Control.DeepSeq (rnf)

import System.IO
import System.Directory

dataDir :: FilePath
dataDir = ".sis"

workDB :: FilePath
workDB = ".sis/workDB.txt"

aliasDB :: FilePath
aliasDB = ".sis/aliasDB.txt"

getFileHandle :: FilePath -> IO Handle
getFileHandle path = do
  exist <- doesFileExist path
  if exist
  then openFile path ReadMode
  else do
    writeFile path ""
    openFile path ReadMode

getContent :: FilePath -> IO String
getContent path = do
  handle' <- getFileHandle path
  contents <- hGetContents handle'
  rnf contents `seq` hClose handle'
  return contents

getId :: String -> IO Int
getId aliasOrId = if all isDigit aliasOrId
                  then return (read aliasOrId :: Int)
                  else do
                    alias <- fmap lines (getContent "aliasDB.txt")
                    let idAndAliasString = map (break isSpace) alias
                        idAndAlias = map (\(idS, alS) -> (read idS :: Int, tail alS)) idAndAliasString
                        validId = filter ((==aliasOrId) . snd) idAndAlias
                    if null validId then return 0 else return (fst . head $ validId)

addWork :: [String] -> IO ()
addWork workNames = do
  contents <- getContent "workDB.txt"
  let idNext = length (lines contents) + 1
      workName = unwords workNames
  appendFile "workDB.txt" ("10" ++ workName ++ "\n")
  putStrLn ("Your work <" ++ workName  ++ "> is now related with id " ++ show idNext ++ ".")

listWorkContents :: IO ()
listWorkContents = do
  contents <- fmap lines (getContent "workDB.txt")
  putStrLn "Id\t| Work"
  let totalWork = length contents
      workList = zip [1..totalWork] contents
      liveWorkList = map (\(id', xs) -> show id' ++ "\t| " ++  drop 2 xs) . filter (\(_, x:_) -> x == '1') $ workList
  putStr (unlines liveWorkList)

aliasWork :: String -> String -> IO ()
aliasWork shortName workId =  do
  contents <- fmap lines (getContent "workDB.txt")
  workId' <- (try:: IO Int -> IO (Either SomeException Int)) (return (read workId :: Int))
  if all isDigit shortName
  then putStrLn "short name cannot be all digits!"
  else case workId' of
    Left _ -> putStrLn "'id' arg is not valid!"
    Right maybeValidId -> do
      let totalWork = length contents
      if maybeValidId > totalWork
      then putStrLn ("Cannot find any work with id " ++ show maybeValidId ++ "!")
      else do
        appendFile "aliasDB.txt" (show maybeValidId ++ " " ++ shortName ++ "\n")
        putStrLn ("'" ++ shortName ++ "' is now linked to work '" ++ tail (contents !! (maybeValidId - 1)) ++ "'.")

removeWork :: String -> IO ()
removeWork idOrAlias = do
  id' <- getId idOrAlias
  if id' == 0
  then putStrLn ("Cannot find and work with id or alias " ++ idOrAlias ++ "!")
  else do
    contents <- fmap lines (getContent "workDB.txt")
    if id' > length contents 
    then putStrLn "Database format error!"
    else do
      let workToRemove = contents !! (id' - 1)
          removedWork = '0' : tail workToRemove
          workStatus = workToRemove !! 1
          newContents = unlines (take (id'-1) contents ++ [removedWork] ++ drop id' contents)
      if workStatus == '1'
      then putStrLn ("'" ++ drop 3 workToRemove ++ "' is still on running, please stop it first.")
      else do
        (tempFileName, handle') <- openTempFile "." "tmpworkdb"
        hPutStr handle' newContents
        hClose handle'
        removeFile "workDB.txt"
        renameFile tempFileName "workDB.txt"
        putStrLn ("Work '" ++ drop 2 removedWork ++ "' has been removed, you can use recover to find it back.")