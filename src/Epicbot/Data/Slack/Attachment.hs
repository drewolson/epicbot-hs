module Epicbot.Data.Slack.Attachment
  ( Attachment,
    cardButton,
    urls,
  )
where

import Data.Aeson (Options (..), ToJSON (..), Value, defaultOptions, genericToJSON)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import qualified Epicbot.Data.Card as Card
import Epicbot.Data.Slack.Action (Action)
import qualified Epicbot.Data.Slack.Action as Action
import GHC.Generics (Generic)

data Attachment = Attachment
  { actions :: Maybe [Action],
    callbackId :: Maybe Text,
    imageUrl :: Text,
    text :: Text
  }
  deriving (Show, Generic)

instance ToJSON Attachment where
  toJSON :: Attachment -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = renameField
        }
    where
      renameField :: String -> String
      renameField = \case
        "callbackId" -> "callback_id"
        "imageUrl" -> "image_url"
        field -> field

cardButton :: Card -> Attachment
cardButton card =
  Attachment
    { actions = Just [Action.fromCard card],
      callbackId = Just "select_card",
      imageUrl = url $ Card.urls card,
      text = Card.name card
    }
  where
    url :: [Text] -> Text
    url = \case
      h : _ -> h
      _ -> ""

urls :: [Text] -> Maybe [Attachment]
urls = \case
  [url] ->
    Just
      [ Attachment
          { text = "",
            imageUrl = url,
            callbackId = Nothing,
            actions = Nothing
          }
      ]
  [front, back] ->
    Just
      [ Attachment
          { text = "Front",
            imageUrl = front,
            callbackId = Nothing,
            actions = Nothing
          },
        Attachment
          { text = "Back",
            imageUrl = back,
            callbackId = Nothing,
            actions = Nothing
          }
      ]
  _ -> Nothing
