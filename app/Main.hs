module Main where

import qualified Bots.Telegram as TG
import qualified Bots.Vk as VK
import Bots.Types (BotType(..), BotConfig(..))
import Config (fromFile)

main :: IO ()
main = do
    config <- fromFile "bot.config"

    case currentBot config of
        Telegram -> TG.start config
        Vk       -> VK.start config
