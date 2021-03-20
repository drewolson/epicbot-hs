module Epicbot.Wiring
  ( buildGlobalEnv,
    buildRequestEnv,
  )
where

import Data.UUID.V4 qualified as UUID
import Epicbot.Data.Env.GlobalEnv (GlobalEnv)
import Epicbot.Data.Env.GlobalEnv qualified as GlobalEnv
import Epicbot.Data.Env.RequestEnv (RequestEnv)
import Epicbot.Data.Env.RequestEnv qualified as RequestEnv
import Epicbot.Data.Index qualified as Index
import Epicbot.Data.OnlineStatus qualified as OnlineStatus
import Epicbot.Data.Slack.SigningSecret qualified as SigningSecret
import Epicbot.Html.Parser qualified as Parser
import Epicbot.Html.Scraper qualified as Scraper
import System.Environment (lookupEnv)

buildGlobalEnv :: IO GlobalEnv
buildGlobalEnv = do
  onlineStatus <- OnlineStatus.fromEnv <$> lookupEnv "EPICBOT_ONLINE"
  signingSecret <- SigningSecret.fromEnv <$> lookupEnv "EPICBOT_SLACK_SIGNING_SECRET"
  port <- maybe 8081 read <$> lookupEnv "EPICBOT_PORT"
  html <- Scraper.scrape onlineStatus
  let cards = Parser.parse html
  let index = Index.fromCards cards

  pure $ GlobalEnv.new index port signingSecret

buildRequestEnv :: IO RequestEnv
buildRequestEnv = RequestEnv.new <$> UUID.nextRandom
