module Config.SisConf where

dataDir :: FilePath
dataDir = ".sis"

workDB :: FilePath
workDB = dataDir ++ "/work.db"

logDB :: FilePath
logDB = dataDir ++ "/log.db"

garDB :: FilePath
garDB = dataDir ++ "/gar.db"