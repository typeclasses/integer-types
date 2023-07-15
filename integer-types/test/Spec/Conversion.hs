module Spec.Conversion where

import Data.Ord qualified as Ord
import Essentials
import Hedgehog qualified
import Integer
  ( BoundedBelow (..),
    Integer,
    IntegerConvert (..),
    IntegerEquiv,
    IntegerNarrow (..),
    Natural,
    Positive,
    Signed,
  )
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (evalMaybe, hedgehog, modifyMaxSuccess, (===))

spec :: Spec
spec = do
  context "convert (convert x) = x" do
    let check ::
          forall a b m.
          (GenIntegral a, IntegerEquiv a b) =>
          Monad m =>
          Hedgehog.PropertyT m ()
        check = do
          x :: a <- Hedgehog.forAll Gen.integral
          convert (convert x :: b) === x

    it "A = Integer, B = Signed" $ hedgehog $ check @Integer @Signed
    it "A = Signed,  B = Integer" $ hedgehog $ check @Signed @Integer

  context "narrow (convert x) = Just x" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, IntegerConvert a b, IntegerNarrow b a) =>
            Monad m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            narrow (convert x :: b) === Just x

      it "A = Natural,  B = Integer" $ hedgehog $ check @Natural @Integer
      it "A = Natural,  B = Signed" $ hedgehog $ check @Natural @Signed
      it "A = Positive, B = Integer" $ hedgehog $ check @Positive @Integer
      it "A = Positive, B = Signed" $ hedgehog $ check @Positive @Signed
      it "A = Positive, B = Natural" $ hedgehog $ check @Positive @Natural

  context "narrow x = (Just y | convert y = x) or Nothing" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, BoundedBelow b) =>
            (IntegerConvert b a, IntegerNarrow a b) =>
            (Show b, Eq b) =>
            Monad m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            let y :: Maybe b = narrow x
            if x Ord.>= convert (minBound @b)
              then do
                z <- evalMaybe y
                convert z === x
              else y === Nothing

      it "A = Integer, B = Natural" $ hedgehog $ check @Integer @Natural
      it "A = Signed,  B = Natural" $ hedgehog $ check @Signed @Natural
      it "A = Integer, B = Positive" $ hedgehog $ check @Integer @Positive
      it "A = Signed,  B = Positive" $ hedgehog $ check @Signed @Positive
      it "A = Natural, B = Positive" $ hedgehog $ check @Natural @Positive
