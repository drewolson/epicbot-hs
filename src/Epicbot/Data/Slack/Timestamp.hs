module Epicbot.Data.Slack.Timestamp
  ( Timestamp,
    isValid,
    fromText,
    fromInt,
    toText,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX qualified as Clock

newtype Timestamp = Timestamp {timestamp :: Int}

isValid :: MonadIO m => Timestamp -> m Bool
isValid Timestamp {timestamp} = do
  now <- round <$> liftIO Clock.getPOSIXTime

  pure $ abs (now - timestamp) < 300

fromInt :: Int -> Timestamp
fromInt = Timestamp

fromText :: Text -> Timestamp
fromText = Timestamp . read . Text.unpack

toText :: Timestamp -> Text
toText = Text.pack . show . timestamp
