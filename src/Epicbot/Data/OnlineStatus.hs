module Epicbot.Data.OnlineStatus
  ( OnlineStatus (..),
    fromEnv,
  )
where

data OnlineStatus
  = Online
  | Offline

fromEnv :: Maybe String -> OnlineStatus
fromEnv = \case
  Just "1" -> Online
  _ -> Offline
