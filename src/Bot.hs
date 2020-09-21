{-# LANGUAGE LambdaCase #-}

module Bot
    ( handleCommand
    ) where

handleCommand :: String -> String
handleCommand = \case
    "" -> "Empty message"
    "/help" -> "Help message"
    "/repeat" -> "Repeat message"
    ('/':_) -> "Unknown command"
    str -> str
