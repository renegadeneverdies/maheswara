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
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Map as Map

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset=" <> (show offset))

getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead a = Just $ head a

sendReply :: Bot -> Integer -> Maybe String -> Message -> Maybe (Bot, Request)
sendReply bot chatId _ msg = message' <|> audio' <|> document' <|> photo' <|> sticker' <|> video' <|> voice'
  where message' = (sendMessage bot chatId mempty) <$> (text msg)
        audio' = (sendAudio bot chatId (caption msg)) <$> (audio msg)
        document' = (sendDocument bot chatId (caption msg)) <$> (document msg)
        photo' = (sendPhoto bot chatId (caption msg)) <$> (photo msg)
        sticker' = (sendSticker bot chatId (caption msg)) <$> (sticker msg)
        video' = (sendVideo bot chatId (caption msg)) <$> (video msg)
        voice' = (sendVoice bot chatId (caption msg)) <$> (voice msg)

sendMessage :: Bot -> Integer -> Maybe String -> SText -> (Bot, Request)
sendMessage bot chatId _ (SText "/help") = (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                <> "&text=" <> (getHelp bot))
sendMessage bot chatId _ (SText "/repeat") = (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                <> "&text=" <> (getRepeat bot))
sendMessage bot chatId _ (SText ('/':x)) = (,) (bot { getUsers = Map.insert chatId (read x) (getUsers bot)}) $
                                     parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                <> "&text=" <> "Reply count set to " <> x)
sendMessage bot chatId _ (SText x) = (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                <> "&text=" <> x)

sendAudio :: Bot -> Integer -> Maybe String -> Media -> (Bot, Request)
sendAudio bot chatId caption audio = (,) bot $ parseRequest_ ((getToken bot) <> "sendAudio?chat_id=" <> (show chatId)
                                             <> "&audio=" <> (file_id audio) <> (fromMaybe mempty (Just "&caption=" <> caption)))

sendDocument :: Bot -> Integer -> Maybe String -> Media -> (Bot, Request)
sendDocument bot chatId caption document = (,) bot $ parseRequest_ ((getToken bot) <> "sendDocument?chat_id=" <> (show chatId)
                                                <> "&document=" <> (file_id document) <> (fromMaybe mempty (Just "&caption=" <> caption)))

sendPhoto :: Bot -> Integer -> Maybe String -> [Media] -> (Bot, Request)
sendPhoto bot chatId caption (x:_) = (,) bot $ parseRequest_ ((getToken bot) <> "sendPhoto?chat_id=" <> (show chatId)
                                             <> "&photo=" <> (file_id x) <> (fromMaybe mempty (Just "&caption=" <> caption)))

sendSticker :: Bot -> Integer -> Maybe String -> Media -> (Bot, Request)
sendSticker bot chatId caption sticker = (,) bot $ parseRequest_ ((getToken bot) <> "sendSticker?chat_id=" <> (show chatId)
                                               <> "&sticker=" <> (file_id sticker) <> (fromMaybe mempty (Just "&caption=" <> caption)))

sendVideo :: Bot -> Integer -> Maybe String -> Media -> (Bot, Request)
sendVideo bot chatId caption video = (,) bot $ parseRequest_ ((getToken bot) <> "sendVideo?chat_id=" <> (show chatId)
                                             <> "&video=" <> (file_id video) <> (fromMaybe mempty (Just "&caption=" <> caption)))

sendVoice :: Bot -> Integer -> Maybe String -> Media -> (Bot, Request)
sendVoice bot chatId caption voice = (,) bot $ parseRequest_ ((getToken bot) <> "sendVoice?chat_id=" <> (show chatId)
                                             <> "&voice=" <> (file_id voice) <> (fromMaybe mempty (Just "&caption=" <> caption)))
