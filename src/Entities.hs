{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Entities where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics
import Data.Map hiding (drop)
import Network.HTTP.Client (Manager(..))

tp = T.pack

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

data RMessage = RMessage
              { message_id :: Integer
              , from :: User
              , date :: Integer
              , chat :: Chat
              , text :: Maybe T.Text
              } deriving (Show, Generic, ToJSON, FromJSON, Eq)

data SMessage = SMessage
              { chat_id' :: Integer
              , text' :: T.Text
              , reply_markup' :: KeyboardMarkup
              } deriving (Show, Eq, Generic)

instance Semigroup SMessage where
  sm1 <> sm2 = SMessage { chat_id' = (chat_id' sm1) + (chat_id' sm2)
                        , text' = (text' sm1) <> (text' sm2)
                        , reply_markup' = reply_markup' sm1 }

instance Monoid SMessage where
  mappend = (<>)
  mempty = SMessage { chat_id' = 0, text' = mempty, reply_markup' = KeyboardMarkup [[]] }

instance FromJSON SMessage where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = init }

instance ToJSON SMessage where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = init }

data Chat = Chat
          { _id :: Integer
          , _type :: T.Text
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
         , getHelp :: T.Text
         , getRepeat :: T.Text
         , getManager :: Manager
         , getToken :: Token
         , getOffset :: Offset
         }

data KeyboardMarkup = KeyboardMarkup
                    { keyboard :: [[KeyboardButton]] } deriving (Show, Eq, ToJSON, FromJSON, Generic)

data KeyboardButton = KeyboardButton
                    { _text :: T.Text} deriving (Show, Eq, Generic)

defaultKeyboard :: KeyboardMarkup
defaultKeyboard = KeyboardMarkup $ [fmap KeyboardButton (fmap T.pack ["/1", "/2", "/3", "/4", "/5"])]

instance FromJSON KeyboardButton where
  parseJSON = genericParseJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance ToJSON KeyboardButton where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }
