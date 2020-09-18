{-# LANGUAGE OverloadedStrings #-}
import Samples
import Entities
import Handlers
import Test.Hspec
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = do
  hspec sendMessageSpec
  hspec composeMessageSpec
  hspec composeMediaSpec

sendMessageSpec :: Spec
sendMessageSpec = do
  describe "sendMessage method tests" $ do
    it "should return updated bot state and request" $
      fmap path (sendMessage sampleBot 1 mempty (T.pack "/2"))
        `shouldBe` (sampleBot { getUsers = M.insert 1 2 M.empty }, C8.pack "/tokenTG/sendMessage")

composeMessageSpec :: Spec
composeMessageSpec = do
  describe "composeMessage method tests" $ do
    it "should create a json value" $ do
      composeMessage 10 (T.pack "text") Nothing
        `shouldBe` (object [ "chat_id" .= (10 :: Integer)
                           , "text" .= T.pack "text"])

    it "should create a json value with keyboard" $ do
      composeMessage 10 (T.pack "text") (Just defaultKeyboard)
        `shouldBe` (object [ "chat_id" .= (10 :: Integer)
                           , "text" .= T.pack "text"
                           , "reply_markup" .= defaultKeyboard ])

composeMediaSpec :: Spec
composeMediaSpec = do
  describe "composeMedia method tests" $ do
    it "should create a photo value" $ do
      composeMedia 10 Nothing (T.pack "photo") (Media "abcd")
        `shouldBe` (object [ "chat_id" .= (10 :: Integer)
                           , "photo" .= T.pack "abcd" ])

    it "should create a photo value with caption" $ do
      composeMedia 10 (Just $ T.pack "caption") (T.pack "photo") (Media "abcd")
        `shouldBe` (object [ "chat_id" .= (10 :: Integer)
                           , "photo" .= T.pack "abcd"
                           , "caption" .= (T.pack "caption") ])

    it "should create a voice value" $ do
      composeMedia 10 Nothing (T.pack "voice") (Media "abcd")
        `shouldBe` (object [ "chat_id" .= (10 :: Integer)
                           , "voice" .= T.pack "abcd" ])
