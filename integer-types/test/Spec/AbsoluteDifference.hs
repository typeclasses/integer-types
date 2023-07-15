module Spec.AbsoluteDifference where

import Essentials
import Hedgehog qualified
import Integer (AbsoluteDifference, Integer, Natural, Positive, Signed, absoluteDifference, yolo)
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude (toInteger, (-))

spec :: Spec
spec =
  context "absoluteDifference @A @B works the same as converting through Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Integer, B = Integer" $ hedgehog $ check @Integer @Integer
      it "A = Natural, B = Natural" $ hedgehog $ check @Natural @Natural
      it "A = Positive, B = Positive" $ hedgehog $ check @Positive @Positive
      it "A = Signed, B = Signed" $ hedgehog $ check @Signed @Signed
      it "A = Positive, B = Natural" $ hedgehog $ check @Positive @Natural
      it "A = Natural, B = Positive" $ hedgehog $ check @Natural @Positive
      it "A = Signed, B = Natural" $ hedgehog $ check @Signed @Natural
      it "A = Natural, B = Signed" $ hedgehog $ check @Natural @Signed
      it "A = Integer, B = Natural" $ hedgehog $ check @Integer @Natural
      it "A = Natural, B = Integer" $ hedgehog $ check @Natural @Integer
      it "A = Signed, B = Positive" $ hedgehog $ check @Signed @Positive
      it "A = Positive, B = Signed" $ hedgehog $ check @Positive @Signed
      it "A = Signed, B = Integer" $ hedgehog $ check @Signed @Integer
      it "A = Integer, B = Signed" $ hedgehog $ check @Integer @Signed
      it "A = Positive, B = Integer" $ hedgehog $ check @Positive @Integer
      it "A = Integer, B = Positive" $ hedgehog $ check @Integer @Positive

check ::
  forall a b m.
  (GenIntegral a, GenIntegral b, AbsoluteDifference a b) =>
  Monad m =>
  Hedgehog.PropertyT m ()
check = do
  x :: a <- Hedgehog.forAll Gen.integral
  y :: b <- Hedgehog.forAll Gen.integral
  absoluteDifference x y === reference (toInteger x) (toInteger y)

reference :: Integer -> Integer -> Natural
reference a b = yolo (if a >= b then a - b else b - a)
