{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.IO
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import Control.Monad
import Entities

fetchJSON :: Request -> IO (Response Value)
fetchJSON = do
  res <- httpJSON
  return res

getUpdates :: Request
getUpdates = "https://api.telegram.org/bot1374635961:AAFqeXWx5nfzseX4FlmGEfSWUQkk70HSqQ8/getUpdates"

handleUpdate :: Update -> Maybe Action
handleUpdate Update {..} = undefined

main :: IO ()
main = do
  response <- fetchJSON getUpdates
  return ()
