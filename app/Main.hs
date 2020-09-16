{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where
import Data.Aeson
import Data.Time
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Types.Status
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Prelude hiding (repeat)
import System.IO ()
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Data.Maybe (fromJust, fromMaybe, isNothing)

import Entities
import Handlers

fetchJSON :: Maybe Request -> Manager -> IO L8.ByteString
fetchJSON (Just req) man = do
  response <- httpLbs req man
  return (responseBody response)
fetchJSON Nothing man    = do
  response <- httpLbs fake man
  return (responseBody response)
  where fake = "https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/getMe"

run :: Manager -> StateT Bot IO ()
run manager = do
  bot <- get
  time <- lift getCurrentTime
  let config = getConfig bot
      action = getAction bot
      token = getTokenTG config
      offset = getOffset bot
      defaultRepeat = getDefault config
  unless (action == Await) (let repeat' = getRepeat' action
                                echo = getEcho action
                             in replicateM repeat' (lift $ httpLbs echo manager)
                               >>= (\(last -> x) -> lift (writeLog time logFile (show (responseStatus x, parseMaybe response' =<< decode (responseBody x))) DEBUG config))
                               >> put (bot { getAction = Await, getOffset = offset + 1 })
                               >> run manager)
  upds <- lift $ fetchJSON (getUpdates offset token) manager
  let list = parseMaybe updates =<< decode upds
  when (list == Just [] || isNothing list) (put (bot { getAction = Await })
                                             >> run manager)
  let currentUpd = head (fromJust list)
      currentMsg = message currentUpd
      chatId = _id (chat currentMsg)
      (newBot, newReq) = fromJust $ sendReply bot chatId mempty currentMsg
      repeats = fromMaybe defaultRepeat (Map.lookup chatId $ getUsers newBot)
  put newBot { getAction = Echo newReq repeats, getOffset = update_id currentUpd }
  run manager

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  time <- getCurrentTime
  file <- T.splitOn (T.pack ";") . T.pack <$> readFile "config/maheswara.config"
  let config = buildConfig $ fmap (T.drop 1) . T.breakOn (T.pack "=") . T.strip <$> file
      bot = Bot { getUsers = Map.empty
                , getAction = Await
                , getOffset = 0
                , getConfig = config
                }
  writeLog time logFile "Bot started " DEBUG config
  let upds = getUpdates 0 (getTokenTG config)
  when (isNothing upds) (writeLog time logFile "Invalid token " ERROR config)
  initial <- fetchJSON (getUpdates 0 (getTokenTG config)) manager
  let offset = maybe 0 update_id (getLast (parseMaybe updates =<< decode initial))
  evalStateT (run manager) (bot { getOffset = offset })


writeLog :: UTCTime -> String -> String -> LogLevel -> Config -> IO ()
writeLog time path str level cfg | level >= getLogLevel cfg = appendFile path (show time <> " " <> show level <> " " <> str <> "\n")
                                 | otherwise = return ()

logFile :: String
logFile = "log/maheswara.log"
