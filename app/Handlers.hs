{-# LANGUAGE OverloadedStrings #-}
module Handlers where
import Entities
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Monoid ((<>))
import Data.Aeson

getUpdates :: Offset -> String -> Request
getUpdates offset token = parseRequest_ (token <> "getUpdates?timeout=5&offset=" <> (show offset))

getContent :: RMessage -> Maybe T.Text
getContent = text

setReply :: Integer -> T.Text -> Maybe KeyboardMarkup -> T.Text -> Maybe T.Text -> Maybe (Bool, SMessage) -- bool indicates if IO is needed
setReply chat repeat' keyboard help mContent = do
  content <- mContent
  case (T.unpack content) of
    "/help" -> return $ (False, SMessage { chat_id' = chat, text' = help, reply_markup' = Nothing })
    "/repeat" -> return $ (True, SMessage { chat_id' = chat, text' = repeat', reply_markup' = keyboard }) -- repeat must be reworked
    _ -> return $ (False, SMessage { chat_id' = chat, text' = content, reply_markup' = Nothing })

sendMessage :: String -> SMessage -> Request
sendMessage token msg = request' { method = "POST"
                                 , requestBody = RequestBodyLBS $ encode msg
                                 , requestHeaders = [("Content-Type","application/json; charset=utf-8")] }
  where request' = parseRequest_ (token <> "sendMessage")

getLast :: Maybe Updates -> Maybe Update
getLast (Just []) = Nothing
getLast (Just x) = Just (last x)
