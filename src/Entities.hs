{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Entities ( User(..)
                , Message(..)
                , Media (..)
                , Action (..)
                , Bot (..)
                , Chat (..)
                , Update (..), Updates, updates
                , KeyboardMarkup(..), KeyboardButton(..), defaultKeyboard
                , UserId, Offset, Token, Repeat) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Map hiding (drop, take)
import Network.HTTP.Client (Request, Manager)
import qualified Data.Text as T

data User = User
          { id' :: Integer
          , is_bot' :: Bool
          , first_name' :: T.Text
          } deriving (Show, Generic, Eq)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = init }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = init }

data Message = Message
              { message_id :: Integer
              , from :: User
              , date :: Integer
              , chat :: Chat
              , text :: Maybe T.Text
              , audio :: Maybe Media
              , document :: Maybe Media
              , photo :: Maybe [Media]
              , sticker :: Maybe Media
              , video :: Maybe Media
              , voice :: Maybe Media
              , caption :: Maybe T.Text
              } deriving (Show, Generic, ToJSON, FromJSON, Eq)

newtype Media = Media { file_id :: T.Text } deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Chat = Chat
          { _id :: Integer
          , _type :: String
          , _first_name :: T.Text
          } deriving (Show, Generic, Eq)

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

data Update = Update
            { update_id :: Integer
            , message :: Message
            } deriving (Show, Generic, ToJSON, FromJSON, Eq)

type Updates = [Update]

updates :: Value -> Parser Updates
updates = withObject "updates" $ \o -> o .: (T.pack "result")

data Action = Await | Echo { getEcho :: Request, getRepeat' :: Repeat } deriving Show

instance Eq Action where
  Await == Await = True
  (Echo _ _) == (Echo _ _) = True
  _ == _ = False

type UserId = Integer
type Offset = Integer
type Repeat = Int
type Token = String

data Bot = Bot
         { getUsers :: (Map UserId Repeat)
         , getAction :: Action
         , getHelp :: T.Text
         , getRepeat :: T.Text
         , getManager :: Manager
         , getToken :: Token
         , getOffset :: Offset
         , getDefault :: Repeat
         }

data KeyboardMarkup = KeyboardMarkup
                    { keyboard :: [[KeyboardButton]] } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data KeyboardButton = KeyboardButton
                    { _text :: T.Text } deriving (Show, Eq, Generic)

defaultKeyboard :: KeyboardMarkup
defaultKeyboard = KeyboardMarkup $ [fmap KeyboardButton (T.pack <$> ["/1", "/2", "/3", "/4", "/5"])]

instance FromJSON KeyboardButton where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance ToJSON KeyboardButton where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }
