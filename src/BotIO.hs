module BotIO
    ( runBot
    , Bot(..)
    ) where

-- Unpure common bot logic

import Control.Exception (catch)

import Network.HTTP.Simple (HttpException(..))

import Logger (logDebug)
import Bot (Env(..), handleCommand)

data Bot = Bot
    { getUpdate :: Env -> IO Env
    , sendMessage :: Env -> String -> IO ()
    }

runBot :: Bot -> Env -> IO ()
runBot bot env = do
    env' <- catch (updateBot bot env) handler
    runBot bot env'
    where
        handler :: HttpException -> IO Env
        handler _ = do
            logDebug "Timeout exception. Resending request..."
            runBot bot env
            return env

updateBot :: Bot -> Env -> IO Env
updateBot bot env = do
    env' <- getUpdate bot env
    let (env'', responses) = handleCommand env' (message env')
    mapM_ (sendMessage bot env'') responses
    return env''
