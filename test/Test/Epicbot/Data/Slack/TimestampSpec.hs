module Test.Epicbot.Data.Slack.TimestampSpec
  ( spec,
  )
where

import Data.Time.Clock.POSIX qualified as Clock
import Epicbot.Data.Slack.Timestamp qualified as Timestamp
import Test.Hspec (Spec, describe, it, parallel, shouldBe)

spec :: Spec
spec = parallel do
  describe "Epicbot.Data.Slack.Timestamp" do
    describe "isValid" do
      it "is true if close to the current time" do
        now <- Timestamp.fromInt . round <$> Clock.getPOSIXTime
        result <- Timestamp.isValid now

        result `shouldBe` True

      it "is false if not close to the current time" do
        now <- Timestamp.fromInt . (+ 500) . round <$> Clock.getPOSIXTime
        result <- Timestamp.isValid now

        result `shouldBe` False
