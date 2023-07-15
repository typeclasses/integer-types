module Spec.Length where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Integer.Natural qualified as Natural
import Integer.Positive qualified as Positive
import Test.Hspec (Spec, context, it, shouldBe)

spec :: Spec
spec =
  context "length" do
    it "Natural" do
      Natural.length "abc" `shouldBe` 3
    it "Positive" do
      Positive.length ('a' :| "bc") `shouldBe` 3
