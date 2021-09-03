module Test.Epicbot.Data.Slack.ActionSpec
  ( spec,
  )
where

import Data.Aeson as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Epicbot.Data.Slack.Action (Action, ActionType)
import Generic.Random (genericArbitrarySingleG, genericArbitraryU, (:+) (..))
import Test.Hspec (Spec, describe, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, liftArbitrary)

genActionType :: Gen ActionType
genActionType = genericArbitraryU

genText :: Gen Text
genText = Text.pack <$> arbitrary

genMaybeText :: Gen (Maybe Text)
genMaybeText = liftArbitrary genText

customGens :: Gen Text :+ Gen (Maybe Text) :+ Gen ActionType
customGens = genText :+ genMaybeText :+ genActionType

genAction :: Gen Action
genAction = genericArbitrarySingleG customGens

spec :: Spec
spec = parallel do
  describe "Epicbot.Data.Slack.Action" do
    describe "json encoding / decoding" do
      prop "correctly round-trips" $
        forAll genAction \action -> do
          let result = Aeson.decode $ Aeson.encode action

          result `shouldBe` Just action
