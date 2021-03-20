module Epicbot.Data.Slack.SigningSecret
  ( SigningSecret,
    fromEnv,
    sign,
  )
where

import Data.Digest.Pure.SHA qualified as SHA
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Encoding qualified as LazyText

newtype SigningSecret = SigningSecret {signingSecret :: Text}
  deriving (Show)

fromEnv :: Maybe String -> SigningSecret
fromEnv = SigningSecret . Text.pack . Maybe.fromMaybe ""

sign :: Text -> SigningSecret -> Text
sign payload SigningSecret {signingSecret} =
  let secretBytes = LazyText.encodeUtf8 $ LazyText.fromStrict signingSecret
      payloadBytes = LazyText.encodeUtf8 $ LazyText.fromStrict payload
   in Text.pack $ SHA.showDigest $ SHA.hmacSha256 secretBytes payloadBytes
