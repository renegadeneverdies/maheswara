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
              } deriving (Show, Eq, Generic)

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

data Action = Await | Echo { getEcho :: SMessage } deriving (Show, Eq)

type UserId = Integer
type Offset = Integer
type Repeat = Integer
type Token = String

data Bot = Bot
         { getUsers :: (Map UserId Repeat)
         , getAction :: Action
         , getHelp :: T.Text
         , getManager :: Manager
         , getToken :: Token
         , getOffset :: Offset
         }
