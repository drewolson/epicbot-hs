module Test.Support.TestApp
  ( TestApp,
    app,
    runTestApp,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random.Class (MonadRandom)
import Control.Natural (type (~>))
import Data.Text (Text)
import Epicbot.Capability.Has (Has (..))
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Capability.MonadSignature (MonadSignature (..))
import Epicbot.Data.Env.GlobalEnv qualified as GlobalEnv
import Epicbot.Data.Index (Index)
import Epicbot.Data.Slack.Signature (Signature)
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import Epicbot.Data.Slack.Timestamp (Timestamp)
import Epicbot.Web.Router qualified as Router
import Epicbot.Wiring qualified as Wiring
import Network.Wai (Application)
import Web.Scotty.Trans (scottyAppT)

newtype TestApp a = TestApp {unTestApp :: IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadRandom
    )

instance MonadSignature TestApp where
  validSignature :: Text -> Maybe Timestamp -> Maybe Signature -> TestApp Bool
  validSignature _ _ _ = pure True

instance Has Index TestApp where
  grab :: TestApp Index
  grab = TestApp $ GlobalEnv.index <$> Wiring.buildGlobalEnv

instance Has SigningSecret TestApp where
  grab :: TestApp SigningSecret
  grab = TestApp $ GlobalEnv.signingSecret <$> Wiring.buildGlobalEnv

instance MonadApp TestApp

app :: IO Application
app = scottyAppT runTestApp Router.router

runTestApp :: TestApp ~> IO
runTestApp = unTestApp
