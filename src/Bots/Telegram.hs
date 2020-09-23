{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bots.Telegram
    ( start
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)

import Data.Aeson (decode)

import qualified Types as T
import qualified Bot as B
import qualified BotIO as BIO
import Config.Types
import Common
import Logger (logDebug)

data UpdateData = UpdateData
    { message :: String
    , chatId   :: Integer
    , updateId :: Integer
    }

start :: BotConfig -> IO ()
start cfg =
    BIO.runBot bot (B.getEnv cfg)
    where bot = BIO.Bot getUpdate sendMessage

getUpdate :: B.Env -> IO B.Env
getUpdate env = do
    let url = B.tgBaseUrl env <> "getUpdates"
        query = [ ("offset", fmap (fromString . show) (B.updateId env))
                , ("timeout", (Just . fromString . show . B.pollTimeout) env)
                ]

    logDebug $ "GET: " <> url
    json <- sendWithQuery query url
    logDebug $ "Response: " <> show json

    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse

    updData <- handleResponse response
    return $ env
        { B.message  = message updData
        , B.chatId   = Just $ chatId updData
        -- We need to increment updateId to set updates as already read
        , B.updateId = (Just . succ . updateId) updData
        }

handleResponse :: Maybe T.TgResponse -> IO UpdateData
handleResponse = \case
    Nothing -> do
        logDebug "handleResponse: No data"
        return $ UpdateData [] (-1) (-1)
    Just response ->
        case T.result response of
            [] -> do
                logDebug "handleResponse: No updates"
                return $ UpdateData [] (-1) (-1)
            xs ->
                let upd = last xs
                    msg = T.getText upd
                    chatId' = T.getChatId upd
                    updateId' = T.updateId upd
                in
                    return $ UpdateData msg chatId' updateId'

sendMessage :: B.Env -> String -> IO ()
sendMessage env msg = do
    logDebug $ "GET: " <> finalUrl
    json <- sendWithQuery query finalUrl
    logDebug $ "Response: " <> show json
    where
        markup = getKeyboardMarkup $ map (show :: Int -> String) [1..5]
        markupM = if B.isAwait env then Just $ fromString markup else Nothing

        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = [ ("chat_id", fmap (fromString . show) (B.chatId env))
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
