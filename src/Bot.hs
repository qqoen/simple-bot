module Bot
    ( handleCommand
    , getEnv
    , Env(..)
    , LogLevel(..)
    ) where

import Logger
import qualified Config.Types as CT

data Env = Env
    { tgBaseUrl :: String
    , helpMsg :: String
    , repeatMsg :: String
    , repeatTimes :: Int
    , updateId :: Maybe Integer
    , logger :: LogLevel -> String -> IO ()
    }

getEnv :: CT.BotConfig -> Env
getEnv cfg = Env
    { tgBaseUrl = "https://api.telegram.org/bot" <> CT.tgToken cfg <> "/"
    , helpMsg = CT.helpMsg cfg
    , repeatMsg = CT.repeatMsg cfg
    , repeatTimes = CT.defaultRepeat cfg
    , updateId = Nothing
    , logger = writeLog Debug
    }

handleCommand :: Env -> String -> String
handleCommand env msg = case msg of
    "/help"   -> helpMsg env
    "/repeat" -> "Currently repeating " <> show (repeatTimes env) <> " times. " <> repeatMsg env
    str       -> str
