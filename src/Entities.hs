{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Entities ( User(..)
                , RMessage(..)
                , SMessage (..)
                , Audio(..)
                , Document(..)
                , PhotoSize (..)
                , Sticker (..)
                , Video (..)
                , Voice (..)
                , Action (..)
                , Bot (..)
                , Chat (..)
                , Update (..), Updates, updates
                , KeyboardMarkup(..), KeyboardButton(..), defaultKeyboard
                , UserId, Offset, Token, Repeat
                , Reply (..)) where

import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics
import Data.Map hiding (drop, take)
import Network.HTTP.Client

tp = T.pack

data User = User
          { id' :: Integer
          , is_bot' :: Bool
          , first_name' :: String
          } deriving (Show, Generic, Eq)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = init }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = init }

data RMessage = RMessage
              { message_id :: Integer
              , from :: User
              , date :: Integer
              , chat :: Chat
              , text :: Maybe SText
              , audio :: Maybe Audio
              , document :: Maybe Document
              , photo :: Maybe Photo
              , sticker :: Maybe Sticker
              , video :: Maybe Video
              , voice :: Maybe Voice
              , caption :: Maybe String
              } deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype SText = SText { _text :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON SText where
  parseJSON = withObject "text" $ \o -> do
    _text <- o .: "text"
    return SText{..}

instance ToJSON SText where
  toJSON SText{..} = object [ "text" .= _text ]

newtype Audio = Audio { file_id :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON Audio where
  parseJSON = withObject "audio" $ \o -> do
    file_id <- o .: "file_id"
    return Audio{..}

instance ToJSON Audio where
  toJSON Audio{..} = object [ "file_id" .= file_id ]

newtype Document = Document { file_id' :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON Document where
  parseJSON = withObject "document" $ \o -> do
    file_id' <- o .: "file_id"
    return Document{..}

instance ToJSON Document where
  toJSON Document{..} = object [ "file_id" .= file_id' ]

newtype PhotoSize = PhotoSize { _file_id :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON PhotoSize where
  parseJSON = withObject "photosize" $ \o -> do
    _file_id <- o .: "file_id"
    return PhotoSize{..}

instance ToJSON PhotoSize where
  toJSON PhotoSize{..} = object [ "file_id" .= _file_id ]
type Photo = [PhotoSize]

newtype Sticker = Sticker { file_id'' :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON Sticker where
  parseJSON = withObject "sticker" $ \o -> do
    file_id'' <- o .: "file_id"
    return Sticker{..}

instance ToJSON Sticker where
  toJSON Sticker{..} = object [ "file_id" .= file_id'' ]

newtype Video = Video { __file_id :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON Video where
  parseJSON = withObject "video" $ \o -> do
    __file_id <- o .: "file_id"
    return Video{..}

instance ToJSON Video where
  toJSON Video{..} = object [ "file_id" .= __file_id ]

newtype Voice = Voice { file_id''' :: String } deriving (Show, Generic, Eq, Semigroup, Monoid)
instance FromJSON Voice where
  parseJSON = withObject "voice" $ \o -> do
    file_id''' <- o .: "file_id"
    return Voice{..}

instance ToJSON Voice where
  toJSON Voice{..} = object [ "file_id" .= file_id''' ]

data SMessage = SMessage
              { chat_id' :: Integer
              , text' :: Maybe SText
              , audio' :: Maybe Audio
              , document' :: Maybe Document
              , photo' :: Maybe [PhotoSize]
              , sticker' :: Maybe Sticker
              , video' :: Maybe Video
              , voice' :: Maybe Voice
              , caption' :: Maybe String
              } deriving (Show, Eq, Generic, Semigroup)

instance Semigroup Integer where -- warning
  int1 <> int2 = int1 + int2

instance FromJSON SMessage where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = init }

instance ToJSON SMessage where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = init }

data Chat = Chat
          { _id :: Integer
          , _type :: String
          , _first_name :: String
          } deriving (Show, Generic, Eq, Semigroup)

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

data Update = Update
            { update_id :: Integer
            , message :: RMessage
            } deriving (Show, Generic, ToJSON, FromJSON, Eq)

type Updates = [Update]

updates :: Value -> Parser Updates
updates = withObject "updates" $ \o -> o .: (tp "result")

data Action = Await | Echo { getEcho :: SMessage, getRepeat' :: Repeat } deriving (Show, Eq)

type UserId = Integer
type Offset = Integer
type Repeat = Int
type Token = String

data Bot = Bot
         { getUsers :: (Map UserId Repeat)
         , getAction :: Action
         , getHelp :: String
         , getRepeat :: String
         , getManager :: Manager
         , getToken :: Token
         , getOffset :: Offset
         }

data KeyboardMarkup = KeyboardMarkup
                    { keyboard :: [[KeyboardButton]] } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data KeyboardButton = KeyboardButton
                    { _text :: String} deriving (Show, Eq, Generic)

defaultKeyboard :: KeyboardMarkup
defaultKeyboard = KeyboardMarkup $ [fmap KeyboardButton ["/1", "/2", "/3", "/4", "/5"]]

instance FromJSON KeyboardButton where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance ToJSON KeyboardButton where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

class Reply a where
  sendReply :: Bot -> Integer -> Maybe String -> a -> Maybe (Bot, Request)

instance Reply RMessage where
  sendReply bot chatId _ RMessage{..} = let sr = sendReply bot chatId caption in
    sr text >>= sr audio




instance Reply SText where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId _ (SText "/help") = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                  <> "&text=" <> (getHelp bot))
  sendReply bot chatId _ (SText "/repeat") = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                  <> "&text=" <> (getRepeat bot))
  sendReply bot chatId _ (SText ('/':x)) = Just $ (,) (bot { getUsers = insert chatId (read x) (getUsers bot)}) $
                                       parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                  <> "&text=" <> "Reply count set to " <> x)
  sendReply bot chatId _ (SText x) = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendMessage?chat_id=" <> (show chatId)
                                                  <> "&text=" <> x)

instance Reply Audio where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption audio = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendAudio?chat_id=" <> (show chatId)
                                                 <> "&audio=" <> (file_id audio) <> (fromMaybe mempty (Just "&caption=" <> caption)))

instance Reply Document where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption document = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendDocument?chat_id=" <> (show chatId)
                                                    <> "&document=" <> (file_id' document) <> (fromMaybe mempty (Just "&caption=" <> caption)))

instance Reply Photo where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption (x:_) = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendPhoto?chat_id=" <> (show chatId)
                                                 <> "&photo=" <> (_file_id x) <> (fromMaybe mempty (Just "&caption=" <> caption)))

instance Reply Sticker where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption sticker = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendSticker?chat_id=" <> (show chatId)
                                                   <> "&sticker=" <> (file_id'' sticker) <> (fromMaybe mempty (Just "&caption=" <> caption)))

instance Reply Video where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption video = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendVideo?chat_id=" <> (show chatId)
                                                 <> "&video=" <> (__file_id video) <> (fromMaybe mempty (Just "&caption=" <> caption)))

instance Reply Voice where
  --sendReply _ _ _ Nothing = Nothing
  sendReply bot chatId caption voice = Just $ (,) bot $ parseRequest_ ((getToken bot) <> "sendVideo?chat_id=" <> (show chatId)
                                                 <> "&voice=" <> (file_id''' voice) <> (fromMaybe mempty (Just "&caption=" <> caption)))
