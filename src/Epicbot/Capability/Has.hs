module Epicbot.Capability.Has
  ( Has,
    grab,
    grabs,
  )
where

import Control.Monad.Trans (lift)
import Web.Scotty.Trans (ActionT, ScottyError)

class Monad m => Has a m where
  grab :: m a

instance (Has a m, ScottyError e) => Has a (ActionT e m) where
  grab :: ActionT e m a
  grab = lift grab

grabs :: Has a m => (a -> b) -> m b
grabs f = f <$> grab
