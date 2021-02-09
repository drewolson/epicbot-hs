module Epicbot.Web.Util
  ( maybeParam,
  )
where

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, Parsable, ScottyError, param, rescue)

maybeParam :: (Parsable a, ScottyError e, Monad m) => Text -> ActionT e m (Maybe a)
maybeParam name = (Just <$> param name) `rescue` const (pure Nothing)
