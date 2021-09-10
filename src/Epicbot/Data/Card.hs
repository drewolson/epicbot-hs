module Epicbot.Data.Card
  ( Card,
    externalId,
    new,
    name,
    urls,
  )
where

import Data.Text (Text)

data Card = Card
  { externalId :: Text,
    name :: Text,
    urls :: [Text]
  }
  deriving (Show, Eq)

new :: Text -> Text -> [Text] -> Card
new = Card
