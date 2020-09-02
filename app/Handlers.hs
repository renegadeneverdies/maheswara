{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns#-}
module Handlers where
import Entities
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8 ()
import Network.HTTP.Client
import Network.HTTP.Client.TLS ()
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Map as Map

getLast :: Maybe Updates -> Maybe Update
getLast Nothing = Nothing
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "/getUpdates?timeout=5&offset=" <> show offset)

sendReply :: Bot -> Integer -> Maybe String -> Message -> Maybe (Bot, Request)
sendReply bot chatId _ msg = message' <|> audio' <|> document' <|> photo' <|> sticker' <|> video' <|> voice'
  where message' = sendMessage bot chatId Nothing <$> text msg
        audio' = sendMedia bot chatId (caption msg) "audio" <$> audio msg
        document' = sendMedia bot chatId (caption msg) "document" <$> document msg
        photo' = sendMedia bot chatId (caption msg) "photo" <$> fmap head (photo msg)
        sticker' = sendMedia bot chatId (caption msg) "sticker" <$> sticker msg
        video' = sendMedia bot chatId (caption msg) "video" <$> video msg
        voice' = sendMedia bot chatId (caption msg) "voice" <$> voice msg

sendMessage :: Bot -> Integer -> Maybe T.Text -> T.Text -> (Bot, Request)
sendMessage bot chatId _ x
  | (T.unpack -> "/help") <- x = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                      , "text" .= getHelp (getConfig bot) ] })
  | (T.unpack -> "/repeat") <- x = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                        , "text" .= getRepeat (getConfig bot)
                                                                                        , "reply_markup" .= defaultKeyboard ] })
  | x' <- x = case T.uncons x' of
                Just ('/', xs) -> (bot { getUsers = Map.insert chatId (read $ T.unpack xs) (getUsers bot) }
                                  , req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                         , "text" .= (T.pack "Reply count set to" <> xs) ] })
                Just _         -> (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                             , "text" .= x ] })
                Nothing        -> (bot, req)
  where req = request' (getTokenTG (getConfig bot)) "/sendMessage"

sendMedia :: Bot -> Integer -> Maybe T.Text -> String -> Media -> (Bot, Request)
sendMedia bot chatId caption name media = (bot, req { requestBody = RequestBodyLBS
                                                    $ encode $ object [ "chat_id" .= chatId
                                                    , T.pack name .= file_id media
                                                    , "caption" .= caption ] })
  where req = request' (getTokenTG (getConfig bot)) ("/send" <> name)

request' :: Token -> String -> Request
request' token path =
  (parseRequest_ $ token <> path) {
   method = "POST",
   requestHeaders = [("Content-Type", "application/json; charset=utf-8")] }
