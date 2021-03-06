module Epicbot.Web.Service.CommandService
  ( handle,
  )
where

import Data.Text (Text)
import Epicbot.Capability.Has (grab)
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Data.Index qualified as Index
import Epicbot.Data.Slack.CommandResponse (CommandResponse)
import Epicbot.Data.Slack.CommandResponse qualified as CommandResponse

handle :: MonadApp m => Text -> m CommandResponse
handle command = do
  index <- grab

  case command of
    "draft" -> CommandResponse.draft <$> Index.random 5 index
    query -> pure $ CommandResponse.fromCards $ Index.search query index
