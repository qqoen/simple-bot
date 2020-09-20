{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getBaseTgUrl
    , fetchJSON
    , getName
    ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.String (fromString)

import Network.HTTP.Simple (httpBS, getResponseBody)
import Control.Lens (preview)
import Data.Aeson.Lens (key, _String)

import ConfigParser


getBaseTgUrl :: IO String
getBaseTgUrl = do
    config <- readConfig "bot.config"
    let token = getValue "TG_BOT_TOKEN" config
    case token of
        Just t -> return ("https://api.telegram.org/bot" <> t <> "/")
        Nothing -> error "Telegram bot token value is not found in config file"

fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
    res <- httpBS (fromString url)
    return $ getResponseBody res

getName :: BS.ByteString -> Maybe Text
getName = preview (key "result" . key "first_name" . _String)
