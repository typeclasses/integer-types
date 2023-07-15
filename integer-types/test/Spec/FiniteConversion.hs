module Spec.FiniteConversion where

import Data.Bool qualified as Bool
import Data.Int (Int)
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Integer
  ( ConvertWithFinite,
    Finite (..),
    Integer,
    IntegerNarrow (..),
    Natural,
    Positive,
    Signed,
  )
import Integer.Gen (GenFinite, GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (PropertyT, forAll, hedgehog, modifyMaxSuccess, (===))
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (fromInteger, toInteger)

spec :: Spec
spec = do
  context "toFinite x = (Just y | fromInteger y = x) or Nothing" $
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Integer,  B = Int " $ hedgehog $ checkToFinite @Integer @Int
      it "A = Integer,  B = Word" $ hedgehog $ checkToFinite @Integer @Word
      it "A = Natural,  B = Int " $ hedgehog $ checkToFinite @Natural @Int
      it "A = Natural,  B = Word" $ hedgehog $ checkToFinite @Natural @Word
      it "A = Positive, B = Int " $ hedgehog $ checkToFinite @Positive @Int
      it "A = Positive, B = Word" $ hedgehog $ checkToFinite @Positive @Word
      it "A = Signed,   B = Int " $ hedgehog $ checkToFinite @Signed @Int
      it "A = Signed,   B = Word" $ hedgehog $ checkToFinite @Signed @Word

  context "fromFinite x = narrow (toInteger x)" do
    it "A = Int,  B = Integer " $ hedgehog $ checkFromFinite @Integer @Int
    it "A = Word, B = Integer" $ hedgehog $ checkFromFinite @Integer @Word
    it "A = Int,  B = Natural " $ hedgehog $ checkFromFinite @Natural @Int
    it "A = Word, B = Natural" $ hedgehog $ checkFromFinite @Natural @Word
    it "A = Int,  B = Positive " $ hedgehog $ checkFromFinite @Positive @Int
    it "A = Word, B = Positive" $ hedgehog $ checkFromFinite @Positive @Word
    it "A = Int,  B = Signed " $ hedgehog $ checkFromFinite @Signed @Int
    it "A = Word, B = Signed" $ hedgehog $ checkFromFinite @Signed @Word

checkToFinite ::
  forall a b m.
  Monad m =>
  (ConvertWithFinite a, GenIntegral a, Show a) =>
  (Integer.Finite b, Eq b, Show b) =>
  PropertyT m ()
checkToFinite = do
  x :: a <- forAll Gen.integral

  let x' = Num.toInteger x
      ok =
        (Bool.&&)
          (x' Ord.>= Num.toInteger (Bounded.minBound :: b))
          (x' Ord.<= Num.toInteger (Bounded.maxBound :: b))

  (Integer.toFinite x :: Maybe b) === if ok then Just (Num.fromInteger x') else Nothing

checkFromFinite ::
  forall a b m.
  Monad m =>
  (ConvertWithFinite a, IntegerNarrow Integer a, Eq a, Show a) =>
  (Finite b, GenFinite b, Show b) =>
  PropertyT m ()
checkFromFinite = do
  x :: b <- forAll Gen.finite
  (Integer.fromFinite x :: Maybe a) === Integer.narrow (Num.toInteger x)
