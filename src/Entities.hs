{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Entities ( User(..)
                , Message(..)
                , Chat(..)
                , Update(..)
                , Action(..)) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics

data User = User
          { id' :: Integer
          , is_bot' :: Bool
          , first_name' :: T.Text
          , last_name' :: Maybe T.Text
          , username' :: T.Text
          , language_code' :: T.Text
          , count' :: Int
          } deriving (Show, Generic)

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
             , text :: T.Text
             } deriving (Show, Generic, ToJSON, FromJSON)

data Chat = Chat
          { _id :: Integer
          , _type :: T.Text
          , _username :: T.Text
          , _first_name :: T.Text
          , _last_name :: Maybe T.Text
          } deriving (Show, Generic)

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

data Action = DoNothing
            | Echo Message
            deriving Show
