module Epicbot.Data.Slack.Signature
  ( Signature,
    isValid,
    fromText,
  )
where

import Data.Text (Text)
import Epicbot.Data.Slack.SigningSecret (SigningSecret)
import Epicbot.Data.Slack.SigningSecret qualified as SigningSecret
import Epicbot.Data.Slack.Timestamp (Timestamp)
import Epicbot.Data.Slack.Timestamp qualified as Timestamp

newtype Signature = Signature {signature :: Text}

fromText :: Text -> Signature
fromText = Signature

isValid :: SigningSecret -> Text -> Timestamp -> Signature -> Bool
isValid signingSecret body timestamp Signature {signature} =
  let payload = "v0:" <> Timestamp.toText timestamp <> ":" <> body
      expectedSignature = "v0=" <> SigningSecret.sign payload signingSecret
   in signature == expectedSignature
