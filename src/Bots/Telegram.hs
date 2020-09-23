{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bots.Telegram
    ( start
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)
import Data.Maybe (isJust)
import Control.Concurrent (threadDelay)
import Control.Exception (catch)

import Data.Aeson (decode)
import Network.HTTP.Simple (HttpException(..))

import qualified Types as T
import qualified Bot as B
import Config.Types
import Common

data UpdateData = UpdateData
    { messages :: [String]
    , chatId :: Integer
    , updateId :: Integer
    }

pollTimeout :: Int
pollTimeout = 60

start :: BotConfig -> IO ()
start cfg = runTgBot $ B.getEnv cfg

runTgBot :: B.Env -> IO ()
runTgBot env = do
    env' <- catch (getUpdates env) handler

    if isJust $ B.updateId env' then
        runTgBot (env' { B.updateId = fmap succ (B.updateId env') })
    else do
        threadDelay $ seconds 3
        runTgBot env'

    where
        handler :: HttpException -> IO B.Env
        handler _ = do
            B.logger env B.Debug "Timeout exception. Resending request..."
            getUpdates env

getUpdates :: B.Env -> IO B.Env
getUpdates env = do
    let url = B.tgBaseUrl env <> "getUpdates"
        query = [ ("offset", fmap (fromString . show) (B.updateId env))
                , ("timeout", (Just . fromString . show) pollTimeout)
                ]

    B.logger env B.Debug $ "GET: " <> url
    json <- sendWithQuery query url
    B.logger env B.Debug $ "Response: " <> show json

    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse

    updData <- handleResponse env response

    let results = map (B.handleCommand env) (messages updData)

    if null results
        then return $ env { B.updateId = Nothing }
        else do
            let lastRes = last results
            handleBotAnswer lastRes updData

handleBotAnswer :: (B.Env, [String]) -> UpdateData -> IO B.Env
handleBotAnswer (env, msgs) updData = do
    mapM_ (sendMessage env (chatId updData)) msgs
    return env { B.updateId = Just $ updateId updData }

handleResponse :: B.Env -> Maybe T.TgResponse -> IO UpdateData
handleResponse env = \case
    Nothing -> do
        B.logger env B.Debug "handleResponse: No data"
        return $ UpdateData [] (-1) (-1)
    Just response ->
        case T.result response of
            [] -> do
                B.logger env B.Debug "handleResponse: No updates"
                return $ UpdateData [] (-1) (-1)
            xs ->
                let msgs = map T.getText xs
                    chatId' = T.getChatId (head xs)
                    updateId' = T.updateId (last xs)
                in
                    return $ UpdateData msgs chatId' updateId'

sendMessage :: B.Env -> Integer -> String -> IO ()
sendMessage env chatId' msg = do
    B.logger env B.Debug $ "GET: " <> finalUrl
    json <- sendWithQuery query finalUrl
    B.logger env B.Debug $ "Response: " <> show json
    where
        markup = getKeyboardMarkup $ map (show :: Int -> String) [1..5]
        markupM = if B.isAwait env then Just $ fromString markup else Nothing

        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = [ ("chat_id", (Just . fromString . show) chatId')
                , ("text", Just $ fromString msg)
                , ("reply_markup", markupM)
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
