module Spec.Increase where

import Control.Monad.Catch (MonadCatch)
import Essentials
import Integer
  ( Increase (..),
    Integer,
    Natural,
    Positive,
    Signed,
    StrictlyIncrease (..),
  )
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (PropertyT, forAll, hedgehog, modifyMaxSuccess, (===))
import Prelude (toInteger, (+))

spec :: Spec
spec = do
  context "increase in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Natural" $ hedgehog $ checkIncrease @Natural
      it "A = Integer" $ hedgehog $ checkIncrease @Integer
      it "A = Positive" $ hedgehog $ checkIncrease @Positive
      it "A = Signed" $ hedgehog $ checkIncrease @Signed

  context "strictlyIncrease in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Natural" $ hedgehog $ checkStrictlyIncrease @Natural
      it "A = Integer" $ hedgehog $ checkStrictlyIncrease @Integer
      it "A = Positive" $ hedgehog $ checkStrictlyIncrease @Positive
      it "A = Signed" $ hedgehog $ checkStrictlyIncrease @Signed

checkIncrease ::
  forall a m.
  (GenIntegral a, Increase a) =>
  MonadCatch m =>
  PropertyT m ()
checkIncrease = do
  x :: Natural <- forAll Gen.integral
  y :: a <- forAll Gen.integral
  toInteger (increase x y) === toInteger x + toInteger y

checkStrictlyIncrease ::
  forall a m.
  (GenIntegral a, StrictlyIncrease a) =>
  MonadCatch m =>
  PropertyT m ()
checkStrictlyIncrease = do
  x :: Positive <- forAll Gen.integral
  y :: a <- forAll Gen.integral
  toInteger (strictlyIncrease x y) === toInteger x + toInteger y
