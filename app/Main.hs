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

import System.IO
import Data.Monoid ((<>))
import Data.Maybe (fromJust, fromMaybe)
import Entities
import Control.Monad
import Control.Monad.State

fetchJSON :: Request -> Manager -> IO L8.ByteString
fetchJSON req man = do
  response <- httpLbs req man
  return (responseBody response)

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset" <> (show offset))

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

-- replace partial functions
getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)
{-
  run :: Manager -> Token -> StateT Bot IO ()
  run manager token = do
    bot <- get
    let offset = if null (getUsers bot) then 0 else

-}
main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  token' <- readFile "token"
  let token = init token'
  response <- fetchJSON (getUpdates token) manager
  let replyTo = message $ getLast (parseMaybe updates =<< decode response)
      newMsg = SMessage { chat_id' = _id (chat replyTo), text' = fromMaybe mempty (text replyTo) }
  response' <- httpLbs (sendMessage token newMsg) manager
  return ()
