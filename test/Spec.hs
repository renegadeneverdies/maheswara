import Samples
import Entities
import Handlers
import Test.Hspec
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Client
import Network.HTTP.Client.TLS

main :: IO ()
main = hspec sendMessageSpec

sendMessageSpec :: Spec
sendMessageSpec = do
  describe "sendMessage method tests" $ do
    it "should return updated bot state and request" $
      fmap path (sendMessage sampleBot 1 mempty (T.pack "/2"))
        `shouldBe` (sampleBot { getUsers = M.insert 1 2 M.empty }, C8.pack "/tokenTG/sendMessage")
