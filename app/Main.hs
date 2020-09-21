module Main where

import Control.Concurrent (threadDelay)

import Lib

main :: IO ()
main = do
    start
    -- 2 seconds
    threadDelay (2 * 1000000)
    main
