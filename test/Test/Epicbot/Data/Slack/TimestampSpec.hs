module Test.Epicbot.Data.Slack.TimestampSpec
  ( spec,
  )
where

import qualified Data.Time.Clock.POSIX as Clock
import qualified Epicbot.Data.Slack.Timestamp as Timestamp
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Epicbot.Data.Slack.Timestamp" $ do
    describe "isValid" $ do
      it "is true if close to the current time" $ do
        now <- Timestamp.fromInt . round <$> Clock.getPOSIXTime
        result <- Timestamp.isValid now

        result `shouldBe` True

      it "is false if not close to the current time" $ do
        now <- Timestamp.fromInt . (+ 500) . round <$> Clock.getPOSIXTime
        result <- Timestamp.isValid now

        result `shouldBe` False
