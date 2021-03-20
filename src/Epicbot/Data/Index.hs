module Epicbot.Data.Index
  ( Index,
    fromCards,
    find,
    random,
    search,
  )
where

import Control.Monad.Random.Class (MonadRandom)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Card qualified as Card
import Epicbot.Data.Index.SearchEngine (SearchEngine)
import Epicbot.Data.Index.SearchEngine qualified as SearchEngine
import System.Random.Shuffle (shuffleM)

data Index = Index
  { cardMap :: Map Text Card,
    searchEngine :: SearchEngine
  }

instance Show Index where
  show :: Index -> String
  show Index {cardMap} = "Index { " <> show cardMap <> " }"

find :: Text -> Index -> Maybe Card
find key = Map.lookup key . cardMap

random :: MonadRandom m => Int -> Index -> m [Card]
random n = fmap (take n) . shuffleM . Map.elems . cardMap

fromCards :: [Card] -> Index
fromCards cards =
  let cardMap = Map.fromList $ fmap (\card -> (Card.externalId card, card)) cards
      searchEngine = SearchEngine.fromCards cards
   in Index {cardMap, searchEngine}

search :: Text -> Index -> [Card]
search term Index {cardMap, searchEngine} =
  mapMaybe (`Map.lookup` cardMap) $ SearchEngine.search term searchEngine
