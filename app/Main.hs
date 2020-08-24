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

getUpdates :: String -> Request
getUpdates token = parseRequest_ (token <> "getUpdates")

getLast :: Maybe Updates -> Update
getLast = last . fromJust

main :: IO ()
main = do
  token <- readFile "token"
  response <- fetchJSON (getUpdates (init token))
  B8.putStrLn $ Yaml.encode (getLast (parseMaybe updates =<< decode response))
