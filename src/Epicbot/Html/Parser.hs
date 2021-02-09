module Epicbot.Html.Parser
  ( parse,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text, isSuffixOf, pack)
import Epicbot.Data.Card (Card)
import qualified Epicbot.Data.Card as Card
import Text.HTML.TagSoup (Tag, fromAttrib, innerText, parseTags, partitions, (~/=), (~==))

parseUrls :: [Tag Text] -> [Text]
parseUrls =
  filter (not . isSuffixOf "scrap-token-back.jpg")
    . filter (not . isSuffixOf "epic_back.jpg")
    . fmap (fromAttrib "src")
    . filter (~== ("<img>" :: String))

parseName :: [Tag Text] -> Text
parseName =
  innerText
    . take 2
    . dropWhile (~/= ("<td class=column-2>" :: String))

parseCard :: (Int, [Tag Text]) -> Maybe Card
parseCard (index, tags) =
  let name = parseName tags
      urls = parseUrls tags
   in if name /= "" && not (null urls)
        then Just $ Card.new (pack $ show index) name urls
        else Nothing

parse :: Text -> [Card]
parse =
  mapMaybe parseCard
    . zip [0 ..]
    . drop 1
    . partitions (~== ("<tr>" :: String))
    . takeWhile (~/= ("</tbody>" :: String))
    . dropWhile (~/= ("<tr>" :: String))
    . parseTags
