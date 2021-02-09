module Epicbot.Web.Service.InteractiveService
  ( handle,
  )
where

import Control.Monad ((<=<))
import Epicbot.Capability.Has (grab)
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Data.Card (Card)
import Epicbot.Data.Index (Index)
import qualified Epicbot.Data.Index as Index
import Epicbot.Data.Slack.CommandResponse (CommandResponse)
import qualified Epicbot.Data.Slack.CommandResponse as CommandResponse
import Epicbot.Data.Slack.InteractivePayload (InteractivePayload)
import qualified Epicbot.Data.Slack.InteractivePayload as InteractivePayload

handle :: MonadApp m => InteractivePayload -> m (Maybe CommandResponse)
handle payload = do
  index <- grab
  let result = findCard index payload

  pure $ CommandResponse.fromCard <$> result

findCard :: Index -> InteractivePayload -> Maybe Card
findCard index = (`Index.find` index) <=< InteractivePayload.firstValue
