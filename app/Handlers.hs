{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handlers where
import Entities
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.Map as Map

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset=" <> (show offset))
{-
getContent :: Integer -> Bot-> RMessage -> (Bot, Request)
getContent chat bot msg = sendReply bot chat caption feed
  where feed = (text msg) <|?> (audio msg) <|?> (document msg) <|?> (sticker msg) <|?> (video msg) <|?> (voice msg)
-}
getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead a = Just $ head a

offset = 19930073
token = "https://api.telegram.org/bot1374635961:AAFqeXWx5nfzseX4FlmGEfSWUQkk70HSqQ8/"
req = getUpdates offset token
