module Logger
    ( Level(..)
    , log
    , logDebug
    , logWarn
    , logError
    ) where

import Prelude hiding (log)

import Data.Char (toUpper)

data Level = Debug | Warn | Error
    deriving (Show, Read, Eq, Ord)

log :: Level -> String -> IO ()
log level msg =
    putStrLn ("[" <> tag <> "] " <> msg <> "\n")
    where tag = map toUpper $ show level

logDebug :: String -> IO ()
logDebug = log Debug

logWarn :: String -> IO ()
logWarn = log Warn

logError :: String -> IO ()
logError = log Error
