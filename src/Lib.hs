{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( start
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Text.IO as TIO

import Network.HTTP.Simple (httpBS, getResponseBody)
import Data.Aeson (decode)

import ConfigParser
import qualified Types as T
import Bot

start :: IO ()
start = getUpdates

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

getUpdates :: IO ()
getUpdates = do
    baseUrl <- getBaseTgUrl
    json <- fetchJSON (baseUrl <> "getUpdates")
    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse
    handleResponse response

handleResponse :: Maybe T.TgResponse -> IO ()
handleResponse Nothing = putStrLn "No data"
handleResponse (Just response) =
    case T.result response of
        [] -> putStrLn "No updates"
        xs ->  
            let msgs = map (handleCommand . T.getText) xs
            in
                mapM_ putStrLn msgs
