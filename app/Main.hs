{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Yaml as Yaml
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

getUpdates :: String -> Request
getUpdates token = parseRequest_ (token <> "getUpdates?timeout=5")

sendMessage :: String -> SMessage -> Request
sendMessage token msg = request' { method = "POST"
                                 , requestBody = RequestBodyLBS $ encode msg
                                 , requestHeaders = [("Content-Type","application/json; charset=utf-8")] }
  where request' = parseRequest_ (token <> "sendMessage")


getLast :: Maybe Updates -> Update
getLast = last . fromJust
{-
run :: StateT HashTable IO ()
run = undefined
-}
main :: IO ()
main = forever $ do
  manager <- newManager tlsManagerSettings
  token' <- readFile "token"
  let token = init token'
  response <- fetchJSON (getUpdates token) manager
  let replyTo = message $ getLast (parseMaybe updates =<< decode response)
      newMsg = SMessage { chat_id' = _id (chat replyTo), text' = fromMaybe mempty (text replyTo) }
  response' <- httpLbs (sendMessage token newMsg) manager
  return ()
