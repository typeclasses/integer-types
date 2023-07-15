module Spec.SubtractOne where

import Essentials
import Hedgehog qualified
import Integer (Positive, Signed)
import Integer.Gen qualified as Gen
import Integer.Positive qualified as Positive
import Integer.Signed qualified as Signed
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude (toInteger, (-))

spec :: Spec
spec =
  context "subtractOne in A behaves the same as (- 1) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Positive" $ hedgehog do
        x :: Positive <- Hedgehog.forAll Gen.integral
        toInteger (Positive.subtractOne x) === toInteger x - 1
      it "A = Signed" $ hedgehog do
        x :: Signed <- Hedgehog.forAll Gen.integral
        toInteger (Signed.subtractOne x) === toInteger x - 1
