{-# LANGUAGE LambdaCase #-}

module HTTP
    ( sendWithQuery
    , exceptionHandler
    ) where

-- HTTP utils and helpers

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS

import Network.HTTP.Simple
    ( HttpException(..)
    , Query
    , httpBS
    , getResponseBody
    , parseRequest_
    , setRequestQueryString
    )
import Network.HTTP.Client.Conduit (HttpExceptionContent(..))

import Logger (logDebug, logError)

sendWithQuery :: MonadIO m => Query -> String -> m BS.ByteString
sendWithQuery query url = do
    res <- httpBS req'
    return $ getResponseBody res
    where
        req = parseRequest_ url
        req' = setRequestQueryString query req

exceptionHandler :: IO () -> HttpException -> IO ()
exceptionHandler continue = \case
    HttpExceptionRequest _ content ->
        case content of
            ResponseTimeout -> do
                logDebug "Timeout exception. Resending request..."
                continue
            _ ->
                logError "HTTP exception"
    InvalidUrlException _ _ ->
        logError "Invalid URL exception"
