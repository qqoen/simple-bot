{-# LANGUAGE OverloadedStrings #-}

-- Types for JSON parsing and config

module Bots.Types
    ( TgResponse(..)
    , TgUpdate(..)
    , TgMessage(..)
    , TgChat(..)
    , BotConfig(..)
    , BotType(..)

    , getText
    , getChatId
    ) where

import Data.Maybe (fromMaybe)

import Data.Aeson

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

data TgResponse = TgResponse 
    { ok :: Bool
    , result :: [TgUpdate]
    } deriving (Show)

data TgUpdate = TgUpdate
    { message :: TgMessage
    , updateId :: Integer
    } deriving (Show)

data TgMessage = TgMessage
    { text :: Maybe String
    , chat :: TgChat
    } deriving (Show)

newtype TgChat = TgChat
    { chatId :: Integer
    } deriving (Show)

instance FromJSON TgResponse where
    parseJSON = withObject "tgResponse" $ \o ->
        TgResponse <$> o .: "ok" <*> o .: "result"

instance FromJSON TgUpdate where
    parseJSON = withObject "tgUpdate" $ \o ->
        TgUpdate <$> o .: "message" <*> o .: "update_id"

instance FromJSON TgMessage where
    parseJSON = withObject "tgMessage" $ \o ->
        TgMessage <$> o .:? "text" <*> o .: "chat"

instance FromJSON TgChat where
    parseJSON = withObject "tgChat" $ \o ->
        TgChat <$> o .: "id"

getText :: TgUpdate -> String
getText = fromMaybe "" . text . message

getChatId :: TgUpdate -> Integer
getChatId = chatId . chat . message
