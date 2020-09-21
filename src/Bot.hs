{--
Common bot functions
--}

module Bot
    ( handleCommand
    , getEnv
    , writeLog
    , Env(..)
    , LogLevel(..)
    ) where

import Data.Char (toUpper)

import ConfigParser

data Env = Env
    { tgChatId :: String
    , tgBaseUrl :: String
    , helpMsg :: String
    , repeatMsg :: String
    , repeatTimes :: Int
    }

data LogLevel = Debug | Warn | Error
    deriving (Show)

configFileName :: String
configFileName = "bot.config"

logFileName :: String
logFileName = "bot.log"

getEnv :: IO Env
getEnv = do
    config <- readConfig configFileName
    let tgToken' = handleValue "TG_BOT_TOKEN" config
    let helpMsg' = handleValue "HELP_MSG" config
    let repeatMsg' = handleValue "REPEAT_MSG" config
    let repeatTimes' = read $ handleValue "DEFAULT_REPEAT" config

    return $ Env
        { tgChatId = ""
        , tgBaseUrl = "https://api.telegram.org/bot" <> tgToken' <> "/"
        , helpMsg = helpMsg'
        , repeatMsg = repeatMsg'
        , repeatTimes = repeatTimes'
        }

handleValue :: String -> Config -> String
handleValue key config =
    case getValue key config of
        Just val -> val
        Nothing -> error $ "Value for " <> key <> " is missing in " <> configFileName

handleCommand :: Env -> String -> String
handleCommand env msg = case msg of
    "/help"   -> helpMsg env
    "/repeat" -> "Currently repeating " <> show (repeatTimes env) <> " times." <> repeatMsg env
    str       -> str

writeLog :: LogLevel -> String -> IO ()
writeLog level msg =
    putStrLn ("[" <> tag <> "] " <> msg <> "\n")
    where tag = map toUpper $ show level
