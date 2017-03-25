module Status.SisStatus where

data SIST = SUCCESS      |
            UNSUCCESS    |
            ID_EXCEED    |
            WORK_REMOVED
            deriving (Show, Eq)

data WKST = Stopped   |
            Started   |
            Paused    |
            Finished
            deriving (Show, Read, Eq, Bounded, Enum)

wkstToInt :: WKST -> Int
wkstToInt = fromEnum

intToWKST :: Int -> WKST
intToWKST = toEnum

