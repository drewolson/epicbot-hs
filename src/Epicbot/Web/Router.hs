module Epicbot.Web.Router
  ( router,
  )
where

import Control.Monad.Trans (MonadIO)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Lazy (Text)
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Data.Slack.InteractivePayload (InteractivePayload)
import Epicbot.Web.Middleware (Middleware)
import Epicbot.Web.Middleware.SignatureCheck qualified as SignatureCheck
import Epicbot.Web.Middleware.SslCheck qualified as SslCheck
import Epicbot.Web.Service.CommandService qualified as CommandService
import Epicbot.Web.Service.InteractiveService qualified as InteractiveService
import Network.HTTP.Types.Status (status404)
import Network.URI.Encode qualified as Encode
import Web.Scotty.Trans (ActionT, ScottyT, json, matchAny, param, raiseStatus)

middlewares :: (MonadIO m, MonadApp m) => Middleware Text m
middlewares = SignatureCheck.call . SslCheck.call

parseInteractivePayload :: MonadApp m => ActionT Text m InteractivePayload
parseInteractivePayload = do
  payload <- ByteString.fromStrict . Encode.decodeByteString <$> param "payload"

  case Aeson.decode payload of
    Nothing -> raiseStatus status404 "not found"
    Just interactivePayload -> pure interactivePayload

router :: (MonadIO m, MonadApp m) => ScottyT Text m ()
router = do
  matchAny "/" $
    middlewares do
      command <- param "text"
      response <- CommandService.handle command

      json response

  matchAny "/interactive" $
    middlewares do
      payload <- parseInteractivePayload
      result <- InteractiveService.handle payload

      case result of
        Nothing -> raiseStatus status404 "not found"
        Just response -> json response
