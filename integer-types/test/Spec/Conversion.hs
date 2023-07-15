module Spec.Conversion where

import Data.Ord qualified as Ord
import Essentials
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
import Test.Hspec.Hedgehog
  ( PropertyT,
    evalMaybe,
    forAll,
    hedgehog,
    modifyMaxSuccess,
    (===),
  )

spec :: Spec
spec = do
  context "convert (convert x) = x" do
    it "A = Integer, B = Signed" $ hedgehog $ checkConvertConvert @Integer @Signed
    it "A = Signed,  B = Integer" $ hedgehog $ checkConvertConvert @Signed @Integer

  context "narrow (convert x) = Just x" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Natural,  B = Integer" $ hedgehog $ checkNarrowConvert @Natural @Integer
      it "A = Natural,  B = Signed" $ hedgehog $ checkNarrowConvert @Natural @Signed
      it "A = Positive, B = Integer" $ hedgehog $ checkNarrowConvert @Positive @Integer
      it "A = Positive, B = Signed" $ hedgehog $ checkNarrowConvert @Positive @Signed
      it "A = Positive, B = Natural" $ hedgehog $ checkNarrowConvert @Positive @Natural

  context "narrow x = (Just y | convert y = x) or Nothing" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Integer, B = Natural" $ hedgehog $ checkConvertNarrow @Integer @Natural
      it "A = Signed,  B = Natural" $ hedgehog $ checkConvertNarrow @Signed @Natural
      it "A = Integer, B = Positive" $ hedgehog $ checkConvertNarrow @Integer @Positive
      it "A = Signed,  B = Positive" $ hedgehog $ checkConvertNarrow @Signed @Positive
      it "A = Natural, B = Positive" $ hedgehog $ checkConvertNarrow @Natural @Positive

checkConvertConvert ::
  forall a b m.
  (GenIntegral a, IntegerEquiv a b) =>
  Monad m =>
  PropertyT m ()
checkConvertConvert = do
  x :: a <- forAll Gen.integral
  convert (convert x :: b) === x

checkNarrowConvert ::
  forall a b m.
  (GenIntegral a, IntegerConvert a b, IntegerNarrow b a) =>
  Monad m =>
  PropertyT m ()
checkNarrowConvert = do
  x :: a <- forAll Gen.integral
  narrow (convert x :: b) === Just x

checkConvertNarrow ::
  forall a b m.
  (GenIntegral a, BoundedBelow b) =>
  (IntegerConvert b a, IntegerNarrow a b) =>
  (Show b, Eq b) =>
  Monad m =>
  PropertyT m ()
checkConvertNarrow = do
  x :: a <- forAll Gen.integral
  let y :: Maybe b = narrow x
  if x Ord.>= convert (minBound @b)
    then do
      z <- evalMaybe y
      convert z === x
    else y === Nothing
