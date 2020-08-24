{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Entities ( User(..)
                , Message(..)
                , Chat(..)
                , Update(..)
                , Action(..)
                , Updates(..)
                , us, m, c, up, ups, updates) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics

tp = T.pack

data User = User
          { id' :: Integer
          , is_bot' :: Bool
          , first_name' :: T.Text
          , last_name' :: Maybe T.Text
          , username' :: T.Text
          , language_code' :: String
          } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = init }

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = init }

us = User { id' = 12, is_bot' = False, first_name' = T.pack "roman", last_name' = Nothing,
           username' = T.pack "rischev", language_code' = "en" }

data Message = Message
             { message_id :: Integer
             , from :: User
             , date :: Integer
             , chat :: Chat
             , text :: T.Text
             } deriving (Show, Generic, ToJSON, FromJSON)

m = Message { message_id = 12345, from = us, date = 1282132989, chat = c, text = T.pack "yo" }

data Chat = Chat
          { _id :: Integer
          , _type :: T.Text
          , _username :: T.Text
          , _first_name :: T.Text
          } deriving (Show, Generic)

c = Chat { _id = 3, _type = T.pack "private", _username = T.pack "rischev",
          _first_name = T.pack "roman" }

instance FromJSON Chat where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance ToJSON Chat where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

data Update = Update
            { update_id :: Integer
            , message :: Message
            } deriving (Show, Generic, ToJSON, FromJSON)

up = Update { update_id = 3, message = m }

type Updates = [Update]

updates :: Value -> Parser Updates
updates = withObject "updates" $ \o -> o .: (tp "result")

ups = [up]

data Action = DoNothing
            | Echo Message
            deriving Show
