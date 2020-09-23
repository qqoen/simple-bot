{-# LANGUAGE LambdaCase #-}

module Bot
    ( handleCommand
    , getEnv
    , isAwait
    , Env(..)
    , LogLevel(..)
    ) where

import Data.Char (isDigit)

import Logger
import qualified Config.Types as CT

data BotMode = Idle | AwaitingRepeatCount

data Env = Env
    { tgBaseUrl :: String
    , helpMsg :: String
    , repeatMsg :: String
    , updateId :: Maybe Integer
    , logger :: LogLevel -> String -> IO ()
    -- Bot state
    , repeatCount :: Int
    , botMode :: BotMode
    }

getEnv :: CT.BotConfig -> Env
getEnv cfg = Env
    { tgBaseUrl = "https://api.telegram.org/bot" <> CT.tgToken cfg <> "/"
    , helpMsg = CT.helpMsg cfg
    , repeatMsg = CT.repeatMsg cfg
    , updateId = Nothing
    , logger = writeLog Debug
    , repeatCount = CT.defaultRepeat cfg
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
