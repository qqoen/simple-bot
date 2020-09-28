{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Bots.Telegram
    ( start
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.String (fromString)
import Control.Exception (catch)

import Data.Aeson (decode)

import qualified Bots.Types as T
import qualified Bot as B
import HTTP
import Logger (logDebug, logError)

start :: T.BotConfig -> IO ()
start cfg =
    catch (B.runBot bot env) (exceptionHandler $ B.runBot bot env)
    where
        env = B.getEnv cfg
        bot = B.Bot getUpdate sendMessage

getUpdate :: B.Env -> IO B.Env
getUpdate env = do
    logDebug $ "GET: " <> url
    json <- sendWithQuery query url
    logDebug $ "Response: " <> show json

    let response = decode (LBS.fromStrict json) :: Maybe T.TgResponse

    (msg, _chatId, updId) <- handleResponse response

    logDebug $ "Got message: " <> msg

    return $ env
        { B.message  = msg
        , B.chatId   = Just _chatId
        -- We need to increment updateId to set updates as already read
        , B.updateId = (Just . succ) updId
        }
    where
        url = B.tgBaseUrl env <> "getUpdates"
        query = [ ("offset", fmap (fromString . show) (B.updateId env))
                , ("timeout", (Just . fromString . show . B.pollTimeout) env)
                ]

sendMessage :: B.Env -> String -> IO ()
sendMessage env msg = do
    logDebug $ "GET: " <> finalUrl
    json <- sendWithQuery query finalUrl
    logDebug $ "Response: " <> show json
    where
        markup = getKeyboardMarkup $ map (show :: Int -> String) [1..5]
        markupM = if B.isAwait env then Just $ fromString markup else Nothing
        -- save string encoding when converting to bytestring
        msg' = encodeUtf8 $ pack msg

        query :: [(BS.ByteString, Maybe BS.ByteString)]
        query = [ ("chat_id", fmap (fromString . show) (B.chatId env))
                , ("text", Just msg')
                , ("reply_markup", markupM)
                ]

        finalUrl = B.tgBaseUrl env <> "sendMessage"

-- Handle parsed json data from getUpdate
handleResponse :: Maybe T.TgResponse -> IO (String, Integer, Integer)
handleResponse = \case
    Nothing -> do
        logDebug "handleResponse: No data"
        return ("", -1, -1)
    Just response ->
        case T.result response of
            [] -> do
                logDebug "handleResponse: No updates"
                return ([], -1, -1)
            xs ->
                let upd = last xs
                    msg = T.getText upd
                    chatId' = T.getChatId upd
                    updateId' = T.updateId upd
                in
                    return (msg, chatId', updateId')

getKeyboardMarkup :: [String] -> String
getKeyboardMarkup msgs =
    "{ \"keyboard\": [[" <> btns <> "]] }"
    where
        fn a b = a ++ "," ++ b
        getBtnMarkup msg = "{ \"text\": " <> msg <> " }"
        btns = foldr1 fn (map getBtnMarkup msgs)
