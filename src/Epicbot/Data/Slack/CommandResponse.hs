module Epicbot.Data.Slack.CommandResponse
  ( CommandResponse,
    fromCards,
    fromCard,
    draft,
  )
where

import Data.Aeson (Options (..), ToJSON (..), Value, defaultOptions, genericToJSON)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Card qualified as Card
import Epicbot.Data.Slack.Attachment (Attachment)
import Epicbot.Data.Slack.Attachment qualified as Attachment
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

data ResponseType
  = Ephemeral
  | InChannel
  deriving (Show, Generic)

instance ToJSON ResponseType where
  toJSON :: ResponseType -> Value
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = quietSnake}

data CommandResponse = CommandResponse
  { attachments :: Maybe [Attachment],
    deleteOriginal :: Maybe Bool,
    responseType :: ResponseType,
    text :: Text
  }
  deriving (Show, Generic)

instance ToJSON CommandResponse where
  toJSON :: CommandResponse -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { omitNothingFields = True,
          fieldLabelModifier = quietSnake
        }

draft :: [Card] -> CommandResponse
draft cards =
  CommandResponse
    { responseType = Ephemeral,
      text = "Draft - what would you pick?",
      attachments = foldMap Attachment.urls $ take 1 . Card.urls <$> cards,
      deleteOriginal = Nothing
    }

fromCards :: [Card] -> CommandResponse
fromCards = \case
  [] ->
    CommandResponse
      { responseType = Ephemeral,
        text = "No card found",
        attachments = Nothing,
        deleteOriginal = Nothing
      }
  [card] -> fromCard card
  cards ->
    CommandResponse
      { responseType = Ephemeral,
        text = "Please select a card",
        attachments = Just (Attachment.cardButton <$> take 3 cards),
        deleteOriginal = Nothing
      }

fromCard :: Card -> CommandResponse
fromCard card =
  CommandResponse
    { responseType = InChannel,
      text = Card.name card,
      attachments = Attachment.urls $ take 2 $ Card.urls card,
      deleteOriginal = Just True
    }
