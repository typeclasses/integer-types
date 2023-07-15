module Spec.Multiplication where

import Essentials
import Integer (Positive, Signed)
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog
  ( PropertyT,
    forAll,
    hedgehog,
    modifyMaxSuccess,
    (===),
  )
import Prelude (fromInteger, toInteger, (*))

spec :: Spec
spec =
  context "(*) behaves the same in A as in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed

check ::
  forall a m.
  GenIntegral a =>
  Monad m =>
  PropertyT m ()
check = do
  x :: a <- forAll Gen.integral
  y :: a <- forAll Gen.integral
  x * y === fromInteger (toInteger x * toInteger y)
