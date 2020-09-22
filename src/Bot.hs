{--
Common bot functions
--}

module Bot
    ( handleCommand
    , getEnv
    , Env(..)
    , LogLevel(..)
    ) where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

import ConfigParser

data LogLevel = Debug | Warn | Error
    deriving (Show, Eq)

data Env = Env
    { tgBaseUrl :: String
    , helpMsg :: String
    , repeatMsg :: String
    , repeatTimes :: Int
    , updateId :: Maybe Integer
    , logger :: LogLevel -> String -> IO ()
    }

instance Ord LogLevel where
    (<=) _ Error   = True
    (<=) Warn Warn = True
    (<=) Debug _   = True

configFileName :: String
configFileName = "bot.config"

-- logFileName :: String
-- logFileName = "bot.log"

getEnv :: IO Env
getEnv = do
    config <- readConfig configFileName
    let tgToken' = handleValue "TG_BOT_TOKEN" config
    let helpMsg' = handleValue "HELP_MSG" config
    let repeatMsg' = handleValue "REPEAT_MSG" config
    let repeatTimes' = read $ handleValue "DEFAULT_REPEAT" config

    return $ Env
        {  tgBaseUrl = "https://api.telegram.org/bot" <> tgToken' <> "/"
        , helpMsg = helpMsg'
        , repeatMsg = repeatMsg'
        , repeatTimes = repeatTimes'
        , updateId = Nothing
        , logger = writeLog Debug
        }

handleValue :: String -> Config -> String
handleValue key config = fromMaybe def res
    where
        res = lookup key config
        def = error $ "Value for " <> key <> " is missing in " <> configFileName

handleCommand :: Env -> String -> String
handleCommand env msg = case msg of
    "/help"   -> helpMsg env
    "/repeat" -> "Currently repeating " <> show (repeatTimes env) <> " times. " <> repeatMsg env
    str       -> str

writeLog :: LogLevel -> LogLevel -> String -> IO ()
writeLog maxLevel level msg =
    when (level >= maxLevel) (putStrLn ("[" <> tag <> "] " <> msg <> "\n"))
    where tag = map toUpper $ show level
