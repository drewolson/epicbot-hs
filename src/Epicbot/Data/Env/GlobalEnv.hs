module Epicbot.Data.Env.GlobalEnv
  ( GlobalEnv,
    new,
    port,
    index,
    signingSecret,
  )
where

import Epicbot.Data.Index (Index)
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import GHC.Generics (Generic)

data GlobalEnv = GlobalEnv
  { index :: Index,
    port :: Int,
    signingSecret :: SigningSecret
  }
  deriving (Generic, Show)

new :: Index -> Int -> SigningSecret -> GlobalEnv
new = GlobalEnv
