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

tp :: String -> T.Text
tp = T.pack
tu :: T.Text -> String
tu = T.unpack

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "/getUpdates?timeout=5&offset=" <> show offset)

getLast :: Maybe Updates -> Maybe Update
getLast Nothing = Nothing
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead a = Just $ head a

sendReply :: Bot -> Integer -> Maybe String -> Message -> Maybe (Bot, Request)
sendReply bot chatId _ msg = message' <|> audio' <|> document' <|> photo' <|> sticker' <|> video' <|> voice'
  where message' = sendMessage bot chatId Nothing <$> text msg
        audio' = sendAudio bot chatId (caption msg) <$> audio msg
        document' = sendDocument bot chatId (caption msg) <$> document msg
        photo' = sendPhoto bot chatId (caption msg) <$> photo msg
        sticker' = sendSticker bot chatId (caption msg) <$> sticker msg
        video' = sendVideo bot chatId (caption msg) <$> video msg
        voice' = sendVoice bot chatId (caption msg) <$> voice msg

sendMessage :: Bot -> Integer -> Maybe T.Text -> T.Text -> (Bot, Request)
sendMessage bot chatId _ x
  | (tu -> "/help") <- x = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                      , "text" .= getHelp (getConfig bot) ] })
  | (tu -> "/repeat") <- x = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                        , "text" .= getRepeat (getConfig bot)
                                                                                        , "reply_markup" .= defaultKeyboard ] })
  | x' <- x = case T.uncons x' of
                Just ('/', xs) -> (bot { getUsers = Map.insert chatId (read $ tu xs) (getUsers bot) }
                                  , req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                         , "text" .= (tp "Reply count set to" <> xs) ] })
                Just _         -> (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                             , "text" .= x ] })
                Nothing        -> (bot, req)
  where req = request' (getTokenTG (getConfig bot)) "/sendMessage"

sendAudio :: Bot -> Integer -> Maybe T.Text -> Media -> (Bot, Request)
sendAudio bot chatId caption (file_id -> audio)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "audio" .= audio
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "audio" .= audio ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendAudio"

sendDocument :: Bot -> Integer -> Maybe T.Text -> Media -> (Bot, Request)
sendDocument bot chatId caption (file_id -> document)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "document" .= document
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "document" .= document ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendDocument"

sendPhoto :: Bot -> Integer -> Maybe T.Text -> [Media] -> (Bot, Request)
sendPhoto bot chatId caption (file_id . head -> photo)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "photo" .= photo
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "photo" .= photo ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendPhoto"

sendSticker :: Bot -> Integer -> Maybe T.Text -> Media -> (Bot, Request)
sendSticker bot chatId caption (file_id -> sticker)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "sticker" .= sticker
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "sticker" .= sticker ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendSticker"

sendVideo :: Bot -> Integer -> Maybe T.Text -> Media -> (Bot, Request)
sendVideo bot chatId caption (file_id -> video)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "video" .= video
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "video" .= video ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendVideo"

sendVoice :: Bot -> Integer -> Maybe T.Text -> Media -> (Bot, Request)
sendVoice bot chatId caption (file_id -> voice)
  | (Just cap) <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                       , "voice" .= voice
                                                                                       , "caption" .= cap ] })
  | Nothing <- caption = (bot, req { requestBody = RequestBodyLBS $ encode $ object [ "chat_id" .= chatId
                                                                                    , "voice" .= voice ] })
  where req = request' (getTokenTG (getConfig bot)) "/sendVoice"

request' :: Token -> String -> Request
request' token path =
  (parseRequest_ $ token <> path) {
   method = "POST",
   requestHeaders = [("Content-Type", "application/json; charset=utf-8")] }
