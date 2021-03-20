module Epicbot
  ( main,
  )
where

import Epicbot.App qualified as App
import Epicbot.Data.Env.GlobalEnv qualified as GlobalEnv
import Epicbot.Web.Router qualified as Router
import Epicbot.Wiring qualified as Wiring
import Web.Scotty.Trans (scottyT)

main :: IO ()
main = do
  globalEnv <- Wiring.buildGlobalEnv

  scottyT (GlobalEnv.port globalEnv) (App.runApp globalEnv) Router.router
