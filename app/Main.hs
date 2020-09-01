{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
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

fetchJSON :: Request -> Manager -> IO L8.ByteString
fetchJSON req man = do
  response <- httpLbs req man
  return (responseBody response)

run :: StateT Bot IO ()
run = do
  bot <- get
  let action = getAction bot
      manager = getManager bot
      token = getTokenTG (getConfig bot)
      offset = getOffset bot
      defaultRepeat = getDefault (getConfig bot)
  unless (action == Await) (replicateM_ (getRepeat' action) (lift $ httpLbs (getEcho action) manager)
                             >> put (bot { getAction = Await, getOffset = offset + 1 })
                             >> run)
  upds <- lift $ fetchJSON (getUpdates offset token) manager
  let list = parseMaybe updates =<< decode upds
  when (list == Just [] || isNothing list) (put (bot { getAction = Await }) >> run)
  let currentUpd = head (fromJust list)
      currentMsg = message currentUpd
      chatId = _id (chat currentMsg)
      (newBot, newReq) = fromJust $ sendReply bot chatId mempty currentMsg
      repeats = fromMaybe defaultRepeat (Map.lookup chatId $ getUsers newBot)
  --lift $ print newReq
  put newBot { getAction = Echo newReq repeats, getOffset = update_id currentUpd }
  run

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  file <- T.splitOn (T.pack ";") . T.pack <$> readFile "../maheswara.config"
  let config = buildConfig $ fmap (T.drop 1) . T.breakOn (T.pack "=") . T.strip <$> file
      bot = Bot { getUsers = Map.empty
                , getAction = Await
                , getManager = manager
                , getOffset = 0
                , getConfig = config
                }
  initial <- fetchJSON (getUpdates 0 (getTokenTG config)) manager
  let offset = maybe 0 update_id (getLast (parseMaybe updates =<< decode initial))
  evalStateT run (bot { getOffset = offset })
