module Epicbot.Data.Index.SearchEngine
  ( SearchEngine,
    fromCards,
    search,
  )
where

import Data.Ix (Ix)
import Data.SearchEngine (NoFeatures, SearchConfig (..), SearchRankParameters (..))
import Data.SearchEngine qualified as SE
import Data.Text (Text)
import Data.Text qualified as Text
import Epicbot.Data.Card (Card)
import Epicbot.Data.Card qualified as Card

newtype SearchEngine = SearchEngine
  { searchEngine :: SE.SearchEngine Card Text SearchField NoFeatures
  }

data SearchField = NameField
  deriving
    ( Eq,
      Ord,
      Enum,
      Bounded,
      Ix,
      Show
    )

search :: Text -> SearchEngine -> [Text]
search query SearchEngine {searchEngine} =
  SE.query searchEngine $ Text.words query

fromCards :: [Card] -> SearchEngine
fromCards cards =
  SearchEngine $ SE.insertDocs cards $ SE.initSearchEngine searchConfig searchRankParams

searchConfig :: SearchConfig Card Text SearchField NoFeatures
searchConfig =
  SearchConfig
    { documentKey = Card.externalId,
      extractDocumentTerms = extractTerms,
      transformQueryTerm = transformQuery,
      documentFeatureValue = const SE.noFeatures
    }

extractTerms :: Card -> SearchField -> [Text]
extractTerms card NameField =
  Text.words $ Text.replace "," "" $ Text.toLower $ Card.name card

transformQuery :: Text -> SearchField -> Text
transformQuery query NameField = Text.replace "," "" $ Text.toLower query

searchRankParams :: SearchRankParameters SearchField NoFeatures
searchRankParams =
  SearchRankParameters
    { paramK1 = 1.5,
      paramB = const 1,
      paramFieldWeights = const 1,
      paramFeatureWeights = SE.noFeatures,
      paramFeatureFunctions = SE.noFeatures,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400,
      paramAutosuggestPrefilterLimit = 500,
      paramAutosuggestPostfilterLimit = 500
    }
