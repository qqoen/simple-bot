module Common
    ( sendWithQuery
    ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString as BS

import Network.HTTP.Simple (Query, httpBS, getResponseBody, parseRequest_, setRequestQueryString)

sendWithQuery :: MonadIO m => Query -> String -> m BS.ByteString
sendWithQuery query url = do
    res <- httpBS req'
    return $ getResponseBody res
    where
        req = parseRequest_ url
        req' = setRequestQueryString query req
