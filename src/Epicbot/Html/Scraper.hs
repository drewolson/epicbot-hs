module Epicbot.Html.Scraper
  ( scrape,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Epicbot.Data.OnlineStatus (OnlineStatus (..))
import qualified Network.HTTP.Simple as HTTP

scrape :: OnlineStatus -> IO Text
scrape = \case
  Online ->
    LazyText.toStrict . LazyText.decodeUtf8 . HTTP.getResponseBody
      <$> HTTP.httpLBS "http://www.epiccardgame.com/card-gallery/"
  Offline ->
    Text.pack <$> readFile "./data/card-gallery.html"
