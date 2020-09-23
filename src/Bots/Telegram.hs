{-# LANGUAGE OverloadedStrings #-}

module Bots.Telegram
    ( start
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)
import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)

import Network.HTTP.Simple (Query, httpBS, getResponseBody, parseRequest_, setRequestQueryString)
import Data.Aeson (decode)

import qualified Types as T
import qualified Bot as B
import Config.Types

data UpdateData = UpdateData
    { messages :: [String]
    , chatId :: Integer
    , updateId :: Integer
    }

start :: BotConfig -> IO ()
start cfg = runTgBot $ B.getEnv cfg

runTgBot :: B.Env -> IO ()
runTgBot env = do
    updId <- getUpdates env

    B.logger env B.Debug ("Update id: " <> show updId)

    if updId /= (-1) then
        void $ getUpdates (env { B.updateId = Just (updId + 1) })
    else
        -- 2 seconds
        void $ threadDelay (2 * 1000000)
        -- runTgBot env

sendWithQuery :: MonadIO m => Query -> String -> m BS.ByteString
sendWithQuery query url = do
    res <- httpBS req'
    return $ getResponseBody res
    where
        req = parseRequest_ url
        req' = setRequestQueryString query req

getUpdates :: B.Env -> IO Integer
getUpdates env = do
    let url = B.tgBaseUrl env
    let updUrl = url <> "getUpdates"

    B.logger env B.Debug $ "GET: " <> updUrl

    let query = [("offset", fmap (fromString . show) (B.updateId env))]
    json <- sendWithQuery query updUrl

    B.logger env B.Debug $ "Response: " <> show json

    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse

    updData <- handleResponse response

    let results = map (B.handleCommand env) (messages updData)

    if null results
        then return ()
        else
            -- TODO: respond to all updates
            sendMessage env (chatId updData) (head results)

    return $ updateId updData

handleResponse :: Maybe T.TgResponse -> IO UpdateData
handleResponse Nothing = do
    putStrLn "handleResponse: No data"
    return $ UpdateData [] (-1) (-1)
handleResponse (Just response) =
    case T.result response of
        [] -> do
            putStrLn "handleResponse: No updates"
            return $ UpdateData [] (-1) (-1)
        xs ->
            let msgs = map T.getText xs
                chatId' = T.getChatId (head xs)
                updateId' = T.update_id (last xs)
            in
                return $ UpdateData msgs chatId' updateId'

sendMessage :: B.Env -> Integer -> String -> IO ()
sendMessage env chatId' msg = do
    B.logger env B.Debug $ "GET: " <> finalUrl
    json <- sendWithQuery query finalUrl
    B.logger env B.Debug $ "Response: " <> show json
    where
        markup = getKeyboardMarkup $ map (show :: Int -> String) [1..5]
        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = [ ("chat_id", (Just . fromString . show) chatId')
                 , ("text", Just $ fromString msg)
                 , ("reply_markup", Just $ fromString markup)
                 ]
        finalUrl = B.tgBaseUrl env <> "sendMessage"

getKeyboardMarkup :: [String] -> String
getKeyboardMarkup msgs =
    "{ \"keyboard\": [[" <> btns <> "]] }"
    where
        fn a b = a ++ "," ++ b
        btns = foldr1 fn (map getBtnMarkup msgs)

getBtnMarkup :: String -> String
getBtnMarkup msg = "{ \"text\": " <> msg <> " }"
