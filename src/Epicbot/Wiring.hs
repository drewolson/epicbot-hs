module Epicbot.Wiring
  ( buildGlobalEnv,
    buildRequestEnv,
  )
where

import qualified Data.UUID.V4 as UUID
import Epicbot.Data.Env.GlobalEnv (GlobalEnv)
import qualified Epicbot.Data.Env.GlobalEnv as GlobalEnv
import Epicbot.Data.Env.RequestEnv (RequestEnv)
import qualified Epicbot.Data.Env.RequestEnv as RequestEnv
import qualified Epicbot.Data.Index as Index
import qualified Epicbot.Data.OnlineStatus as OnlineStatus
import qualified Epicbot.Data.Slack.SigningSecret as SigningSecret
import qualified Epicbot.Html.Parser as Parser
import qualified Epicbot.Html.Scraper as Scraper
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
