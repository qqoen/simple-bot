module Config.Types
    ( BotConfig(..)
    , BotType(..)
    ) where

import Logger

data BotType = Telegram | Vk

data BotConfig = BotConfig
    { helpMsg :: String
    , repeatMsg :: String
    , defaultRepeat :: Int
    , tgToken :: String
    , vkToken :: String
    , logLevel :: Level
    , logConsole :: Bool
    , currentBot :: BotType
    }
