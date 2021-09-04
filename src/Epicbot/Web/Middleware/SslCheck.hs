module Epicbot.Web.Middleware.SslCheck
  ( call,
  )
where

import Data.Text.Lazy (Text)
import Epicbot.Web.Middleware (Middleware)
import Epicbot.Web.Util (maybeParam)
import Web.Scotty.Trans (text)

call :: Monad m => Middleware Text m
call next = do
  value <- maybeParam "ssl_check"

  if value == Just ("1" :: Text)
    then text "SSL Check OK"
    else next
