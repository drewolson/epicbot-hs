module Epicbot.Web.Router
  ( router,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Text.Lazy (Text)
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Data.Slack.InteractivePayload (InteractivePayload)
import Epicbot.Web.Middleware (Middleware)
import qualified Epicbot.Web.Middleware.SignatureCheck as SignatureCheck
import qualified Epicbot.Web.Middleware.SslCheck as SslCheck
import qualified Epicbot.Web.Service.CommandService as CommandService
import qualified Epicbot.Web.Service.InteractiveService as InteractiveService
import Network.HTTP.Types.Status (status404)
import qualified Network.URI.Encode as Encode
import Web.Scotty.Trans (ActionT, ScottyT, json, matchAny, param, raiseStatus)

middlewares :: MonadApp m => Middleware Text m
middlewares = SignatureCheck.call . SslCheck.call

parseInteractivePayload :: MonadApp m => ActionT Text m InteractivePayload
parseInteractivePayload = do
  payload <- ByteString.fromStrict . Encode.decodeByteString <$> param "payload"

  case Aeson.decode payload of
    Nothing -> raiseStatus status404 "not found"
    Just interactivePayload -> pure interactivePayload

router :: MonadApp m => ScottyT Text m ()
router = do
  matchAny "/" $
    middlewares $ do
      command <- param "text"
      response <- CommandService.handle command

      json response

  matchAny "/interactive" $
    middlewares $ do
      payload <- parseInteractivePayload
      result <- InteractiveService.handle payload

      case result of
        Nothing -> raiseStatus status404 "not found"
        Just response -> json response
