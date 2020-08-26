{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Prelude hiding (repeat)
import System.IO
import Data.Monoid ((<>))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Entities
import Control.Monad
import Control.Monad.State

fetchJSON :: Request -> Manager -> IO L8.ByteString
fetchJSON req man = do
  response <- httpLbs req man
  return (responseBody response)

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset=" <> (show offset))

-- test version. Proposal: Content type
getContent :: RMessage -> Maybe T.Text
getContent = text

setReply :: Integer -> T.Text -> T.Text -> Maybe T.Text -> Maybe (Bool, SMessage) -- bool indicates if IO is needed
setReply chat repeat' help mContent = do
  content <- mContent
  case (T.unpack content) of
    "/help" -> return $ (False, SMessage { chat_id' = chat, text' = help })
    "/repeat" -> return $ (True, SMessage { chat_id' = chat, text' = repeat' }) -- repeat must be reworked
    _ -> return $ (False, SMessage { chat_id' = chat, text' = content })

sendMessage :: String -> SMessage -> Request
sendMessage token msg = request' { method = "POST"
                                 , requestBody = RequestBodyLBS $ encode msg
                                 , requestHeaders = [("Content-Type","application/json; charset=utf-8")] }
  where request' = parseRequest_ (token <> "sendMessage")

getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)

run :: StateT Bot IO ()
run = do
  bot <- get
  let users = getUsers bot
      action = getAction bot
      help = getHelp bot
      manager = getManager bot
      token = getToken bot
      offset = getOffset bot
  unless (action == Await) (lift (httpLbs (sendMessage token (getEcho action)) manager)
                             >> put (bot { getAction = Await, getOffset = offset + 1 })
                             >> run)
  upds <- lift $ fetchJSON (getUpdates offset token) manager
  let list = parseMaybe updates =<< decode upds
  when (list == Just []) (put (bot { getAction = Await }) >> run)
  let current = head (fromJust list)
      content = getContent (message current)
      newMsg = SMessage { chat_id' = _id (chat $ message current), text' = fromJust content }
  put bot { getAction = Echo newMsg, getOffset = update_id current }
  run

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  token' <- readFile "token"
  help' <- readFile "help"
  repeat' <- readFile "repeat"
  let token = init token'
      help = T.pack (init help')
      repeat = T.pack (init repeat')
      bot = Bot { getUsers = Map.empty
                , getAction = Await
                , getHelp = help
                , getManager = manager
                , getToken = token
                , getOffset = 0 }
  initial <- fetchJSON (getUpdates 0 token) manager
  let offset = fromMaybe 0 $ fmap update_id (getLast (parseMaybe updates =<< decode initial))
  evalStateT run (bot { getOffset = offset })
