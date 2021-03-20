module Epicbot.Data.Slack.InteractivePayload
  ( InteractivePayload,
    fromCards,
    firstValue,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Slack.Action (Action)
import Epicbot.Data.Slack.Action qualified as Action
import GHC.Generics (Generic)

newtype InteractivePayload = InteractivePayload
  { actions :: [Action]
  }
  deriving (Show, Eq, Generic)

instance FromJSON InteractivePayload

instance ToJSON InteractivePayload

fromCards :: [Card] -> InteractivePayload
fromCards = InteractivePayload . fmap Action.fromCard

firstValue :: InteractivePayload -> Maybe Text
firstValue = \case
  InteractivePayload {actions = action : _} -> Action.value action
  _ -> Nothing
