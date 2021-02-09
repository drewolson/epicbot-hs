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
    genericToJSON,
    withObject,
    withText,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import qualified Epicbot.Data.Card as Card
import GHC.Generics (Generic)

data ActionType = Button | Checkboxes
  deriving (Eq, Show, Generic)

instance FromJSON ActionType where
  parseJSON :: Value -> Parser ActionType
  parseJSON = withText "ActionType" $ \case
    "button" -> pure Button
    "checkboxes" -> pure Checkboxes
    _ -> fail "unknown action type"

instance ToJSON ActionType where
  toJSON :: ActionType -> Value
  toJSON = genericToJSON $ defaultOptions {constructorTagModifier = renameTag}
    where
      renameTag :: String -> String
      renameTag = \case
        "Button" -> "button"
        "Checkboxes" -> "checkboxes"
        tag -> tag

data Action = Action
  { name :: Maybe Text,
    text :: Maybe Text,
    type' :: ActionType,
    value :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Action where
  parseJSON :: Value -> Parser Action
  parseJSON = withObject "Action" $ \obj -> do
    name <- obj .:? "name"
    text <- obj .:? "text"
    type' <- parseJSON =<< obj .: "type"
    value <- obj .:? "value"

    pure Action {name, text, type', value}

instance ToJSON Action where
  toJSON :: Action -> Value
  toJSON =
    genericToJSON $
      defaultOptions
        { fieldLabelModifier = renameType,
          omitNothingFields = True
        }
    where
      renameType :: String -> String
      renameType = \case
        "type'" -> "type"
        field -> field

fromCard :: Card -> Action
fromCard card =
  Action
    { name = Just "select",
      text = Just "select",
      type' = Button,
      value = Just $ Card.externalId card
    }
