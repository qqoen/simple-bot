{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( TgResponse(..)
    , TgUpdate(..)
    , TgMessage(..)
    , TgChat(..)
    , getText
    , getChatId
    ) where

import Prelude (Bool(..), Show(..), Integer(..), String(..), (.), ($), (<$>), (<*>))

import Data.Aeson

data TgResponse = TgResponse 
    { ok :: Bool
    , result :: [TgUpdate]
    } deriving (Show)

data TgUpdate = TgUpdate
    { message :: TgMessage
    } deriving (Show)

data TgMessage = TgMessage
    { text :: String
    , chat :: TgChat
    } deriving (Show)

data TgChat = TgChat
    { id :: Integer
    } deriving (Show)

getText :: TgUpdate -> String
getText = text . message

getChatId :: TgUpdate -> Integer
getChatId = id . chat . message

instance FromJSON TgResponse where
    parseJSON = withObject "tgResponse" $ \o ->
        TgResponse <$> o .: "ok" <*> o .: "result"

instance FromJSON TgUpdate where
    parseJSON = withObject "tgUpdate" $ \o ->
        TgUpdate <$> o .: "message"

instance FromJSON TgMessage where
    parseJSON = withObject "tgMessage" $ \o ->
        TgMessage <$> o .: "text" <*> o .: "chat"

instance FromJSON TgChat where
    parseJSON = withObject "tgChat" $ \o ->
        TgChat <$> o .: "id"
