{-# LANGUAGE LambdaCase #-}

-- Pure common bot logic

module Bot
    ( handleCommand
    , getEnv
    , isAwait
    , runBot

    , Env(..)
    , Bot(..)
    ) where

import Prelude hiding (log)
import Data.Char (isDigit)

import qualified Bots.Types as BT

data BotMode = Idle | AwaitingRepeatCount

data Env = Env
    { helpMsg :: String
    , repeatMsg :: String
    , pollTimeout :: Int
    , message :: String
    -- Tg specific
    , tgBaseUrl :: String
    , updateId :: Maybe Integer
    , chatId :: Maybe Integer
    -- Bot state
    , repeatCount :: Int
    , botMode :: BotMode
    }

data Bot m = Bot
    { getUpdate :: Env -> m Env
    , sendMessage :: Env -> String -> m ()
    }

runBot :: Monad m => Bot m -> Env -> m ()
runBot bot env = do
    env' <- updateBot bot env
    runBot bot env'

updateBot :: Monad m => Bot m -> Env -> m Env
updateBot bot env = do
    env' <- getUpdate bot env
    let (env'', responses) = handleCommand env' (message env')
    mapM_ (sendMessage bot env'') responses
    return env''

getEnv :: BT.BotConfig -> Env
getEnv cfg = Env
    { helpMsg = BT.helpMsg cfg
    , repeatMsg = BT.repeatMsg cfg
    , pollTimeout = 100
    , message = ""
    , tgBaseUrl = "https://api.telegram.org/bot" <> BT.tgToken cfg <> "/"
    , updateId = Nothing
    , chatId = Nothing
    , repeatCount = BT.defaultRepeat cfg
    , botMode = Idle
    }

isAwait :: Env -> Bool
isAwait env = case botMode env of
    Idle                -> False
    AwaitingRepeatCount -> True

handleCommand :: Env -> String -> (Env, [String])
handleCommand env = case botMode env of
    Idle                -> handleIdle env
    AwaitingRepeatCount -> handleAwait env

handleIdle :: Env -> String -> (Env, [String])
handleIdle env = \case
    []        -> (env, [])
    "/help"   -> (env, [helpMsg env])
    "/repeat" ->
        let env' = env { botMode = AwaitingRepeatCount }
            msg = "Currently repeating " <> show (repeatCount env) <> " times. " <> repeatMsg env
        in
            (env', [msg])
    msg       -> (env, replicate (repeatCount env) msg)

handleAwait :: Env -> String -> (Env, [String])
handleAwait env []  = (env, [])
handleAwait env msg =
    if isInt
        then
            let repeatCount' = read msg :: Int
                env' = env { repeatCount = repeatCount', botMode = Idle }
            in 
                if repeatCount' >= 1 && repeatCount' <= 5
                    then (env', ["Repeat count is set to " <> show repeatCount'])
                    else errResponse
        else
            errResponse
    where
        isInt = all isDigit msg
        errResponse = (env, ["Please enter a number from 1 to 5"])
