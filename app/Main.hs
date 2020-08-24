{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens (key, _String)
import Data.String.Conversions (cs)
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.IO
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens (preview)
import Entities

fetchJSON :: Request -> IO L8.ByteString
fetchJSON = (getResponseBody `fmap`) . httpLBS

getUpdates :: Request
getUpdates = "https://api.telegram.org/bot1374635961:AAFqeXWx5nfzseX4FlmGEfSWUQkk70HSqQ8/getUpdates"

getLast :: Maybe Updates -> Update
getLast = last . fromJust

main :: IO Update
main = do
  response <- fetchJSON getUpdates
  return $ getLast (parseMaybe updates =<< decode response)

--a = "chat":{"id":334428388,"first_name":"roman","username":"renegadeneverdies","type":"private"}
