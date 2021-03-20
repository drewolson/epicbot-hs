module Epicbot.Web.Service.InteractiveService
  ( handle,
  )
where

import Control.Monad ((<=<))
import Epicbot.Capability.Has (grab)
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Index (Index)
import Epicbot.Data.Index qualified as Index
import Epicbot.Data.Slack.CommandResponse (CommandResponse)
import Epicbot.Data.Slack.CommandResponse qualified as CommandResponse
import Epicbot.Data.Slack.InteractivePayload (InteractivePayload)
import Epicbot.Data.Slack.InteractivePayload qualified as InteractivePayload

handle :: MonadApp m => InteractivePayload -> m (Maybe CommandResponse)
handle payload = do
  index <- grab
  let result = findCard index payload

  pure $ CommandResponse.fromCard <$> result

findCard :: Index -> InteractivePayload -> Maybe Card
findCard index = (`Index.find` index) <=< InteractivePayload.firstValue
