module Samples where
import Entities
import qualified Data.Text as T
import qualified Data.Map as Map

sampleBot :: Bot
sampleBot = Bot { getUsers = Map.empty
                , getAction = Await
                , getConfig = sampleConfig
                , getOffset = 0 }

sampleConfig :: Config
sampleConfig = Config { getTokenTG = "https://api.telegram.org/tokenTG"
                      , getTokenVK = "tokenVK"
                      , getLogLevel = DEBUG
                      , getHelp = T.pack "Help"
                      , getRepeat = T.pack "Repeat"
                      , getDefault = 1 }
