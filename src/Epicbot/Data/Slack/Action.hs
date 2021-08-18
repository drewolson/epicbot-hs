module Epicbot.Data.Slack.Action
  ( Action,
    ActionType (..),
    fromCard,
    value,
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    Value,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Card qualified as Card
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

data ActionType = Button | Checkboxes
  deriving (Eq, Show, Generic)

actionTypeJSONOptions :: Options
actionTypeJSONOptions = defaultOptions {constructorTagModifier = quietSnake}

instance FromJSON ActionType where
  parseJSON :: Value -> Parser ActionType
  parseJSON = genericParseJSON actionTypeJSONOptions

instance ToJSON ActionType where
  toJSON :: ActionType -> Value
  toJSON = genericToJSON actionTypeJSONOptions

data Action = Action
  { name :: Maybe Text,
    text :: Maybe Text,
    type' :: ActionType,
    value :: Maybe Text
  }
  deriving (Eq, Show, Generic)

actionJSONOptions :: Options
actionJSONOptions =
  defaultOptions
    { fieldLabelModifier = filter (/= '\''),
      omitNothingFields = True
    }

instance FromJSON Action where
  parseJSON :: Value -> Parser Action
  parseJSON = genericParseJSON actionJSONOptions

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON = genericToJSON actionJSONOptions

fromCard :: Card -> Action
fromCard card =
  Action
    { name = Just "select",
      text = Just "select",
      type' = Button,
      value = Just $ Card.externalId card
    }
