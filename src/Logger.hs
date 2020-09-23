module Logger
    ( LogLevel(..)
    , writeLog
    ) where

import Data.Char (toUpper)
import Control.Monad (when)

data LogLevel = Debug | Warn | Error
    deriving (Show, Read, Eq, Ord)

writeLog :: LogLevel -> LogLevel -> String -> IO ()
writeLog maxLevel level msg =
    when (level >= maxLevel) (putStrLn ("[" <> tag <> "] " <> msg <> "\n"))
    where tag = map toUpper $ show level
