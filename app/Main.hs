{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.String (fromString)

import Network.HTTP.Simple (httpBS, getResponseBody)
import Control.Lens (preview)
import Data.Aeson.Lens (key, _String)

import Lib

token = ""
baseUrl = "https://api.telegram.org/bot" <> token <> "/"

main :: IO ()
main = do
    json <- fetchJSON (baseUrl <> "getMe")
    case getRate json of
        Just name -> TIO.putStrLn ("Bot name: " <> name)
        Nothing   -> TIO.putStrLn "No data"

fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
    res <- httpBS (fromString url)
    return $ getResponseBody res

getRate :: BS.ByteString -> Maybe Text
getRate = preview (key "result" . key "first_name" . _String)
