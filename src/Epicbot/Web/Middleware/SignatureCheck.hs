module Epicbot.Web.Middleware.SignatureCheck
  ( call,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Epicbot.Capability.MonadSignature (MonadSignature)
import Epicbot.Capability.MonadSignature qualified as MonadSignature
import Epicbot.Data.Slack.Signature qualified as Signature
import Epicbot.Data.Slack.Timestamp qualified as Timestamp
import Epicbot.Web.Middleware (Middleware)
import Network.HTTP.Types.Status (unauthorized401)
import Web.Scotty.Trans (ActionT, body, header, status, text)

call :: MonadIO m => MonadSignature m => Middleware Text m
call next = do
  maybeSignature <- fmap (Signature.fromText . Text.toStrict) <$> header "X-Slack-Signature"
  maybeTimestamp <- fmap (Timestamp.fromText . Text.toStrict) <$> header "X-Slack-Request-Timestamp"
  bodyText <- Text.toStrict . Text.decodeUtf8 <$> body
  result <- MonadSignature.validSignature bodyText maybeTimestamp maybeSignature

  if result then next else unauthorized

unauthorized :: Monad m => ActionT Text m ()
unauthorized = do
  status unauthorized401
  text "unauthorized"
