module Epicbot.Data.Slack.SigningSecret
  ( SigningSecret,
    fromEnv,
    sign,
  )
where

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

newtype SigningSecret = SigningSecret {signingSecret :: Text}
  deriving (Show)

fromEnv :: Maybe String -> SigningSecret
fromEnv = SigningSecret . Text.pack . Maybe.fromMaybe ""

sign :: Text -> SigningSecret -> Text
sign payload SigningSecret {signingSecret} =
  let secretBytes = LazyText.encodeUtf8 $ LazyText.fromStrict signingSecret
      payloadBytes = LazyText.encodeUtf8 $ LazyText.fromStrict payload
   in Text.pack $ SHA.showDigest $ SHA.hmacSha256 secretBytes payloadBytes
