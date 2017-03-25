module Data.SisDb where

import IO.SisIO (createFile)
import Config.SisConf
import Status.SisStatus
import Data.SisDBStr

import Control.Monad (unless)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (createDirectoryIfMissing, doesFileExist)

initDB :: IO Connection
initDB = do
  createDirectoryIfMissing True dataDir
  workDBExist <- doesFileExist workDB
  unless workDBExist $ do
    createFile workDB
    conn <- connectSqlite3 workDB
    changed <- withTransaction conn (\c -> run c createWorkDBStr [])
    disconnect conn
    unless (changed == 0) $ putStrLn "Error happend when creating database!"
  connectSqlite3 workDB

addNewWorkIntoDB :: Connection -> String -> IO ()
addNewWorkIntoDB conn work = run conn insertWorkDBStr [toSql work]

setAliasIntoDB :: Connection -> Integer -> String -> IO SIST
setAliasIntoDB conn id' alias = do
  valid <- verifyIDBound conn id'
  if valid == SUCCESS
  then do
    changed <- withTransaction conn (\c -> run c setAliasDBStr [toSql id', toSql alias])
    verifyChangedLines changed 1
  else return valid

setWorkStatus :: Connection -> Integer -> WKST -> IO SIST
setWorkStatus conn id' st = do
  valid <- verifyIDBound conn id'
  if valid == SUCCESS
  then do
    changed <- withTransaction conn (\c -> run c setStatusDBStr [toSql id', toSql (wkstToInt st)])
    verifyChangedLines changed 1
  else return valid
  
getNextWorkID :: Connection -> IO Integer
getNextWorkID conn = do
  res <- quickQuery' conn "SELECT LAST_INSERT_ROWID() FROM work" []
  if null res || null (head res)
  then return 0
  else return (fromSql (head (head res)))::Integer

verifyIDBound :: Connection -> Integer -> IO SIST
verifyIDBound conn id' = do
  maxId <- getNextWorkID conn
  if id' <= 0 || id' > maxId
  then return ID_EXCEED
  else return SUCCESS

verifyChangedLines :: Integer -> Integer -> IO SIST
verifyChangedLines line1 line2 = if line1 == line2 then return SUCCESS else return UNSUCCESS