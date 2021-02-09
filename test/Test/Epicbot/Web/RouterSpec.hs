module Test.Epicbot.Web.RouterSpec
  ( spec,
  )
where

import Data.Aeson (Value, (.:))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as ByteString
import Data.Maybe (fromMaybe)
import qualified Epicbot.Data.Card as Card
import qualified Epicbot.Data.Slack.InteractivePayload as InteractivePayload
import qualified Network.URI.Encode as Encode
import Network.Wai.Test (simpleBody)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai (get, liftIO, postHtmlForm, shouldRespondWith, with)
import qualified Test.Support.TestApp as TestApp
import Text.RawString.QQ (r)

decodeAttachments :: ByteString -> [Value]
decodeAttachments bs = fromMaybe [] $ do
  result <- Aeson.decode bs
  flip parseMaybe result $ \obj -> do
    obj .: "attachments"

spec :: Spec
spec =
  describe "Epicbot.Web.Router" $ do
    describe "router" $ do
      with TestApp.app $ do
        it "handles post ssl checks" $ do
          postHtmlForm "/" [("ssl_check", "1")]
            `shouldRespondWith` "SSL Check OK"

        it "handles get ssl checks" $ do
          get "/?ssl_check=1"
            `shouldRespondWith` "SSL Check OK"

        it "handles interactive requests" $ do
          let card = Card.new "1" "Thought Plucker" ["http://www.epiccardgame.com/wp-content/uploads/2015/09/thought_plucker-215x300.jpg"]
          let payload = InteractivePayload.fromCards [card]
          let encoded = Encode.encode $ ByteString.toString $ Aeson.encode payload

          postHtmlForm "/interactive" [("payload", encoded)]
            `shouldRespondWith` [r|{"delete_original":true,"attachments":[{"text":"","image_url":"https://www.epiccardgame.com/wp-content/uploads/2017/11/PNA-EN-03-angeline-s-favor.jpg"}],"text":"Angeline's Favor","response_type":"in_channel"}|]

        describe "commands" $ do
          it "handles searches" $ do
            postHtmlForm "/" [("text", "plucker")]
              `shouldRespondWith` [r|{"delete_original":true,"attachments":[{"text":"","image_url":"http://www.epiccardgame.com/wp-content/uploads/2015/09/thought_plucker-215x300.jpg"}],"text":"Thought Plucker","response_type":"in_channel"}|]

          it "handles drafts" $ do
            body <- simpleBody <$> postHtmlForm "/" [("text", "draft")]
            let attachments = decodeAttachments body

            liftIO $ length attachments `shouldBe` 5
