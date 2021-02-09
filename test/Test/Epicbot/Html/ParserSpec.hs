module Test.Epicbot.Html.ParserSpec
  ( spec,
  )
where

import qualified Epicbot.Data.Card as Card
import Epicbot.Data.OnlineStatus (OnlineStatus (..))
import qualified Epicbot.Html.Parser as Parser
import qualified Epicbot.Html.Scraper as Scraper
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Epicbot.Html.Parser" $ do
    describe "parse" $ do
      it "parses a list of cards" $ do
        cards <- Parser.parse <$> Scraper.scrape Offline

        length cards `shouldBe` 292

        let card = head cards

        Card.externalId card `shouldBe` "0"
        Card.name card `shouldBe` "Angeline, Silver Wing"
        Card.urls card `shouldBe` ["https://www.epiccardgame.com/wp-content/uploads/2017/11/PNA-EN-01-angeline.jpg"]