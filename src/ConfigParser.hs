module ConfigParser
    ( readConfig
    , getValue
    ) where

import System.IO
import Data.List (find)

type Config = [(String, String)]

readConfig :: String -> IO Config
readConfig fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    return $ parse contents

getValue :: String -> Config -> Maybe String
getValue key config =
    case find ((==key) . fst) config of
        Just (key', val) -> Just val
        Nothing -> Nothing

parse :: String -> Config
parse content =
    let 
        lines' = filter (not . null) (lines content)
    in
        filter isValidEntry $ map parseLine lines'

parseLine :: String -> (String, String)
parseLine line = 
    let (key, val) = break (=='=') line
    in
        (key, drop 1 val)

isValidEntry :: (String, String) -> Bool
isValidEntry ("", _) = False
isValidEntry (_, "") = False
isValidEntry _       = True
