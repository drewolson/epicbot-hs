module Epicbot.Web.Middleware
  ( Middleware,
  )
where

import Web.Scotty.Trans (ActionT)

type Middleware e m = (ActionT e m () -> ActionT e m ())
