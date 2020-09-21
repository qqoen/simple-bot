{-# LANGUAGE OverloadedStrings #-}

module Types
    ( TgResponse(..)
    , TgUpdate(..)
    , TgMessage(..)
    , getText
    ) where

import Data.Aeson

data TgResponse = TgResponse 
    { ok :: Bool
    , result :: [TgUpdate]
    } deriving Show

data TgUpdate = TgUpdate
    { message :: TgMessage
    } deriving Show

data TgMessage = TgMessage
    { text :: String
    } deriving Show

getText :: TgUpdate -> String
getText = text . message

instance FromJSON TgResponse where
    parseJSON = withObject "tgResponse" $ \o ->
        TgResponse <$> o .: "ok" <*> o .: "result"

instance FromJSON TgUpdate where
    parseJSON = withObject "tgUpdate" $ \o ->
        TgUpdate <$> o .: "message"

instance FromJSON TgMessage where
    parseJSON = withObject "tgMessage" $ \o ->
        TgMessage <$> o .: "text"
