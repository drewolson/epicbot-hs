{-# OPTIONS_GHC -fno-warn-orphans #-}

module Epicbot.Capability.MonadApp
  ( MonadApp,
  )
where

import Control.Monad.Random.Class (MonadRandom (..))
import Control.Monad.Trans.Class (lift)
import Epicbot.Capability.Has (Has)
import Epicbot.Capability.MonadSignature (MonadSignature)
import Epicbot.Data.Index (Index)
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import Web.Scotty.Trans (ActionT, ScottyError)

class
  ( Monad m,
    MonadRandom m,
    Has Index m,
    Has SigningSecret m,
    MonadSignature m
  ) =>
  MonadApp m

instance (MonadRandom m, ScottyError e) => MonadRandom (ActionT e m) where
  getRandom = lift getRandom
  getRandoms = lift getRandoms
  getRandomR = lift . getRandomR
  getRandomRs = lift . getRandomRs

instance (MonadApp m, ScottyError e) => MonadApp (ActionT e m)
