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
start = do
    (baseUrl, chatId) <- getTgData
    getUpdates baseUrl

getTgData :: IO (String, String)
getTgData = do
    config <- readConfig "bot.config"
    let token = getValue "TG_BOT_TOKEN" config
    let chatId = getValue "TG_CHAT_ID" config

    case token of
        Just t ->
            case chatId of
                Just id -> return ("https://api.telegram.org/bot" <> t <> "/", id)
                Nothing -> error "Telegram chat id value is not found in config file"
        Nothing -> error "Telegram bot token value is not found in config file"

fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
    res <- httpBS (fromString url)
    return $ getResponseBody res

getUpdates :: String -> IO ()
getUpdates url = do
    json <- fetchJSON (url <> "getUpdates")
    BS.putStrLn json
    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse
    (msgs, chatId) <- handleResponse response
    if null msgs
        then return ()
        else sendMessage url chatId (head msgs)

handleResponse :: Maybe T.TgResponse -> IO ([String], String)
handleResponse Nothing = do
    putStrLn "handleResponse: No data"
    return ([], "")
handleResponse (Just response) =
    case T.result response of
        [] -> do
            putStrLn "handleResponse: No updates"
            return ([], "")
        xs ->
            let msgs = map (handleCommand . T.getText) xs
                chatId = T.getChatId (head xs)
            in
                return (msgs, show chatId)

sendMessage :: String -> String -> String -> IO ()
sendMessage url chatId msg = do
    res <- httpBS $ fromString (url <> "sendMessage?chat_id=" <> chatId <> "&text=" <> msg)
    let json = getResponseBody res
    BS.putStrLn json
