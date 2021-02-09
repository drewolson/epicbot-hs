module Epicbot.Data.Env.RequestEnv
  ( RequestEnv,
    new,
  )
where

import Data.UUID (UUID)
import GHC.Generics (Generic)

newtype RequestEnv = RequestEnv
  { requestId :: UUID
  }
  deriving (Generic, Show)

new :: UUID -> RequestEnv
new requestId = RequestEnv {requestId}
