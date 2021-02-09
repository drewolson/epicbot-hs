module Epicbot
  ( main,
  )
where

import qualified Epicbot.App as App
import qualified Epicbot.Data.Env.GlobalEnv as GlobalEnv
import qualified Epicbot.Web.Router as Router
import qualified Epicbot.Wiring as Wiring
import Web.Scotty.Trans (scottyT)

main :: IO ()
main = do
  globalEnv <- Wiring.buildGlobalEnv

  scottyT (GlobalEnv.port globalEnv) (App.runApp globalEnv) Router.router
