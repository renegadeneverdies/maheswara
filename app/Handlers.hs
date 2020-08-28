{-# LANGUAGE OverloadedStrings #-}
module Handlers where
import Entities
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.Map as Map

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset=" <> (show offset))

getContent :: RMessage -> Maybe T.Text
getContent = text

setReply :: Integer -> Bot-> Maybe T.Text -> Maybe (Repeat, SMessage) -- bool indicates if IO is need
setReply chat bot mContent = do
  content <- mContent
  let handleInput :: String -> (Repeat, SMessage)
      handleInput "/help" = (userRepeat, newMsg { text' = getHelp bot, reply_markup' = KeyboardMarkup [[]] })
      handleInput "/repeat" = (userRepeat, newMsg { text' = getRepeat bot, reply_markup' = defaultKeyboard }) -- repeat must be reworked
      handleInput ('/':x) = (read x :: Repeat, newMsg { text' = T.pack ("Reply count set to " ++ x), reply_markup' = KeyboardMarkup [[]] })
      handleInput _ = (userRepeat, newMsg { text' = content, reply_markup' = KeyboardMarkup [[]] })
      newMsg = SMessage { chat_id' = chat }
      userRepeat = fromMaybe 1 (Map.lookup chat (getUsers bot))
  return $ handleInput (T.unpack content)

sendMessage :: String -> SMessage -> Request
sendMessage token msg = request' { method = "POST"
                                 , requestBody = RequestBodyLBS $ encode msg
                                 , requestHeaders = [("Content-Type","application/json; charset=utf-8")] }
  where request' = parseRequest_ (token <> "sendMessage")

getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)
