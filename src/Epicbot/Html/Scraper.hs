module Epicbot.Html.Scraper
  ( scrape,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText
import Epicbot.Data.OnlineStatus (OnlineStatus (..))
import Network.HTTP.Simple qualified as HTTP

scrape :: OnlineStatus -> IO Text
scrape = \case
  Online ->
    LazyText.toStrict . LazyText.decodeUtf8 . HTTP.getResponseBody
      <$> HTTP.httpLBS "http://www.epiccardgame.com/card-gallery/"
  Offline ->
    Text.pack <$> readFile "./data/card-gallery.html"
