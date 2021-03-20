module Epicbot.App
  ( App,
    runApp,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Text (Text)
import Epicbot.Capability.Has (Has (..))
import Epicbot.Capability.MonadApp (MonadApp)
import Epicbot.Capability.MonadSignature (MonadSignature (..))
import Epicbot.Data.Env (Env)
import Epicbot.Data.Env qualified as Env
import Epicbot.Data.Env.GlobalEnv (GlobalEnv)
import Epicbot.Data.Index (Index)
import Epicbot.Data.Slack.Signature (Signature)
import Epicbot.Data.Slack.Signature qualified as Signature
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import Epicbot.Data.Slack.Timestamp (Timestamp)
import Epicbot.Data.Slack.Timestamp qualified as Timestamp
import Epicbot.Wiring qualified as Wiring

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadRandom,
      MonadReader Env
    )

instance MonadSignature App where
  validSignature :: Text -> Maybe Timestamp -> Maybe Signature -> App Bool
  validSignature body (Just timestamp) (Just signature) = do
    isValidTimestamp <- Timestamp.isValid timestamp
    signingSecret <- grab
    let isValidSignature = Signature.isValid signingSecret body timestamp signature

    pure $ isValidTimestamp && isValidSignature
  validSignature _ _ _ = pure False

instance Has Index App where
  grab :: App Index
  grab = asks Env.index

instance Has SigningSecret App where
  grab :: App SigningSecret
  grab = asks Env.signingSecret

instance MonadApp App

runApp :: GlobalEnv -> App a -> IO a
runApp globalEnv app = do
  requestEnv <- Wiring.buildRequestEnv
  let env = Env.new globalEnv requestEnv

  runReaderT (unApp app) env
