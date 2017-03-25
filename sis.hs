module Main where

import Database.HDBC (disconnect)
import System.Environment (getArgs)
import Core.SisCore
import Data.SisDB (initDB)

main :: IO ()
main = do
  conn <- initDB
  args <- getArgs
  case args of 
    ["help"] -> putStrLn ("\n Use 'hub [option]', following options are provided:                    \n\n" ++ 
                         "   add [work name]: add a new work type into database.                    \n\n" ++
                         "   list : list all work type with ids.                                    \n\n" ++
                         "   alias [short name] [id]: link a short name to your specified work.     \n\n" ++
                         "   remove [id | short name]: remove a specified work from work set.       \n\n" ++
                         "   recover [id | short name]: recover a removed work.                     \n\n" ++
                         "   start [id | short name]: start a specified work.                       \n\n" ++
                         "   pause [id | short name]: pause a started work.                         \n\n" ++
                         "   continue [id | short name]: continue a paused work.                    \n\n" ++
                         "   stop [id | short name]: end a started or paused work, you can have     \n"   ++
                         "      some comments on this work.                                         \n\n" ++
                         "   status [id | short name]: show current working status.                 \n\n" ++
                         "   detail [id | short name]: list detail log for specified work.          \n\n" ++
                         "   help: show this message                                                \n")
    ("add": workNames) -> addWork workNames
    ["list"] -> listWorkContents
    ["alias", shortName, workId] -> aliasWork shortName workId
    ["remove", idOrAlias] -> removeWork idOrAlias
    _ -> putStrLn "Functions not implemented yet"
  disconnect conn