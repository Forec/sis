module Data.SisDBStr where

createWorkDBStr :: String
createWorkDBStr = "CREATE TABLE work (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, "   ++ 
                                     "status INT(2), "                                   ++ 
                                     "content VARCHAR(2047), "                           ++
                                     "alias VARCHAR(127))"

insertWorkDBStr :: String
insertWorkDBStr = "INSERT INTO work (content) VALUES (?)"

setAliasDBStr :: String
setAliasDBStr = "UPDATE WORK SET alias = ? WHERE id = ?"

setStatusDBStr :: String
setStatusDBStr = "UPDATE WORK SET status = ? WHERE id = ?"