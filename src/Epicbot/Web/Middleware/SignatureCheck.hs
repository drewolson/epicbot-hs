module Epicbot.Web.Middleware.SignatureCheck
  ( call,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Epicbot.Capability.MonadSignature (MonadSignature)
import qualified Epicbot.Capability.MonadSignature as MonadSignature
import qualified Epicbot.Data.Slack.Signature as Signature
import qualified Epicbot.Data.Slack.Timestamp as Timestamp
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
