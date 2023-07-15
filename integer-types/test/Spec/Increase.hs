module Spec.Increase where

import Control.Monad.Catch qualified as Exception (MonadCatch)
import Essentials
import Hedgehog qualified
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
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude (toInteger, (+))

spec :: Spec
spec = do
  context "increase in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, Increase a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: Natural <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            toInteger (increase x y) === toInteger x + toInteger y

      it "A = Natural" $ hedgehog $ check @Natural
      it "A = Integer" $ hedgehog $ check @Integer
      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed

  context "strictlyIncrease in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, StrictlyIncrease a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: Positive <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            toInteger (strictlyIncrease x y) === toInteger x + toInteger y

      it "A = Natural" $ hedgehog $ check @Natural
      it "A = Integer" $ hedgehog $ check @Integer
      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed
