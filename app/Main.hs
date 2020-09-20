{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO

import Lib

main :: IO ()
main = do
    baseUrl <- getBaseTgUrl
    json <- fetchJSON (baseUrl <> "getMe")
    case getName json of
        Just name -> TIO.putStrLn ("Bot name: " <> name)
        Nothing   -> TIO.putStrLn "No data"
