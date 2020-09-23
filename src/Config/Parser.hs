module Config.Parser
    ( fromFile
    , Entry
    ) where

import System.IO
import Data.Maybe (fromMaybe)

import Config.Types

type Entry = (String, String)

fromFile :: String -> IO BotConfig
fromFile fn = do
    entries <- loadEntries fn
    return $ BotConfig
        { helpMsg = lookupEntry "HELP_MSG" entries
        , repeatMsg = lookupEntry "REPEAT_MSG" entries
        , defaultRepeat = read $ lookupEntry "DEFAULT_REPEAT" entries
        , tgToken = lookupEntry "TG_BOT_TOKEN" entries
        , vkToken = lookupEntry "VK_BOT_TOKEN" entries
        , logLevel = read $ lookupEntry "LOG_LEVEL" entries
        , logConsole = read $ lookupEntry "LOG_CONSOLE" entries
        , currentBot = Telegram
        }

lookupEntry :: String -> [Entry] -> String
lookupEntry key entries =
    fromMaybe fallback $ lookup key entries
    where fallback = error $ key <> " value is missing in config file"

loadEntries :: String -> IO [Entry]
loadEntries fileName = do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    return $ parse contents

parse :: String -> [Entry]
parse content =
    let 
        lines' = filter (not . null) (lines content)
    in
        filter isValidEntry $ map parseLine lines'

parseLine :: String -> Entry
parseLine line = 
    let (key, val) = break (=='=') line
    in
        (key, drop 1 val)

isValidEntry :: Entry -> Bool
isValidEntry ("", _) = False
isValidEntry (_, "") = False
isValidEntry _       = True
