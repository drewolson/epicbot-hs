module Epicbot.Data.Env
  ( Env,
    new,
    index,
    requestId,
    signingSecret,
  )
where

import Control.Lens ((^.))
import Data.Generics.Product (field)
import Data.UUID (UUID)
import Epicbot.Data.Env.GlobalEnv (GlobalEnv)
import Epicbot.Data.Env.RequestEnv (RequestEnv)
import Epicbot.Data.Index (Index)
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import GHC.Generics (Generic)

data Env = Env
  { globalEnv :: GlobalEnv,
    requestEnv :: RequestEnv
  }
  deriving (Generic, Show)

new :: GlobalEnv -> RequestEnv -> Env
new globalEnv requestEnv = Env {globalEnv, requestEnv}

index :: Env -> Index
index env = env ^. field @"globalEnv" ^. field @"index"

requestId :: Env -> UUID
requestId env = env ^. field @"requestEnv" ^. field @"requestId"

signingSecret :: Env -> SigningSecret
signingSecret env = env ^. field @"globalEnv" ^. field @"signingSecret"
