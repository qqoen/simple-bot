{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startTgBot
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)
import Control.Concurrent (threadDelay)

import Network.HTTP.Simple (httpBS, getResponseBody)
import Data.Aeson (decode)

import qualified Types as T
import Bot

startTgBot :: IO ()
startTgBot = do
    env <- getEnv
    getUpdates env
    -- 2 seconds
    threadDelay (2 * 1000000)
    -- startTgBot

fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
    res <- httpBS (fromString url)
    return $ getResponseBody res

getUpdates :: Env -> IO ()
getUpdates env = do
    let url = tgBaseUrl env
    let updUrl = url <> "getUpdates"

    writeLog Debug $ "GET: " <> updUrl
    json <- fetchJSON updUrl
    writeLog Debug $ "Response: " <> show json

    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse

    (msgs, chatId) <- handleResponse env response

    if null msgs
        then return ()
        else sendMessage url chatId (head msgs)

handleResponse :: Env -> Maybe T.TgResponse -> IO ([String], String)
handleResponse _ Nothing = do
    putStrLn "handleResponse: No data"
    return ([], "")
handleResponse env (Just response) =
    case T.result response of
        [] -> do
            putStrLn "handleResponse: No updates"
            return ([], "")
        xs ->
            let msgs = map (handleCommand env . T.getText) xs
                chatId = T.getChatId (head xs)
            in
                return (msgs, show chatId)

sendMessage :: String -> String -> String -> IO ()
sendMessage url chatId msg = do
    writeLog Debug $ "GET: " <> finalUrl
    res <- httpBS $ fromString finalUrl

    let json = getResponseBody res

    writeLog Debug $ "Response: " <> show json

    where
        markup = getKeyboardMarkup $ map show [1..5]
        params = [("chat_id", chatId), ("text", msg)] --, ("reply_markup", markup)]
        finalUrl = url <> "sendMessage" <> getQuery params

getQuery :: [(String, String)] -> String
getQuery params =
    let
        mapFn (key, val) = key <> "=" <> val
        foldFn a b = a ++ "&" ++ b
    in
        "?" <> foldr1 foldFn (map mapFn params)

getKeyboardMarkup :: [String] -> String
getKeyboardMarkup msgs =
    "[[" <> btns <> "]]"
    where
        fn a b = a ++ "," ++ b
        btns = foldr1 fn (map getBtnMarkup msgs)

getBtnMarkup :: String -> String
getBtnMarkup msg = "{ \"text\": " <> msg <> " }"
