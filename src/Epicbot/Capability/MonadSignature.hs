module Epicbot.Capability.MonadSignature
  ( MonadSignature,
    validSignature,
  )
where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import Epicbot.Data.Slack.Signature (Signature)
import Epicbot.Data.Slack.Timestamp (Timestamp)
import Web.Scotty.Trans (ActionT, ScottyError)

class Monad m => MonadSignature m where
  validSignature :: Text -> Maybe Timestamp -> Maybe Signature -> m Bool

instance (MonadSignature m, ScottyError e) => MonadSignature (ActionT e m) where
  validSignature :: Text -> Maybe Timestamp -> Maybe Signature -> ActionT e m Bool
  validSignature body timestamp signature = lift $ validSignature body timestamp signature
