module Spec.FiniteConversion where

import Data.Bool qualified as Bool
import Data.Int (Int)
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Hedgehog qualified
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
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (fromInteger, toInteger)

spec :: Spec
spec = do
  context "toFinite x = (Just y | fromInteger y = x) or Nothing" $
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            Monad m =>
            (ConvertWithFinite a, GenIntegral a, Show a) =>
            (Integer.Finite b, Eq b, Show b) =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            let x' = Num.toInteger x
            let ok =
                  x' Ord.>= Num.toInteger (Bounded.minBound :: b)
                    Bool.&& x' Ord.<= Num.toInteger (Bounded.maxBound :: b)
            (Integer.toFinite x :: Maybe b)
              === if ok then Just (Num.fromInteger x') else Nothing

      it "A = Integer,  B = Int " $ hedgehog $ check @Integer @Int
      it "A = Integer,  B = Word" $ hedgehog $ check @Integer @Word
      it "A = Natural,  B = Int " $ hedgehog $ check @Natural @Int
      it "A = Natural,  B = Word" $ hedgehog $ check @Natural @Word
      it "A = Positive, B = Int " $ hedgehog $ check @Positive @Int
      it "A = Positive, B = Word" $ hedgehog $ check @Positive @Word
      it "A = Signed,   B = Int " $ hedgehog $ check @Signed @Int
      it "A = Signed,   B = Word" $ hedgehog $ check @Signed @Word

  context "fromFinite x = narrow (toInteger x)" do
    let check ::
          forall a b m.
          Monad m =>
          (ConvertWithFinite a, IntegerNarrow Integer a, Eq a, Show a) =>
          (Finite b, GenFinite b, Show b) =>
          Hedgehog.PropertyT m ()
        check = do
          x :: b <- Hedgehog.forAll Gen.finite
          (Integer.fromFinite x :: Maybe a) === Integer.narrow (Num.toInteger x)

    it "A = Int,  B = Integer " $ hedgehog $ check @Integer @Int
    it "A = Word, B = Integer" $ hedgehog $ check @Integer @Word
    it "A = Int,  B = Natural " $ hedgehog $ check @Natural @Int
    it "A = Word, B = Natural" $ hedgehog $ check @Natural @Word
    it "A = Int,  B = Positive " $ hedgehog $ check @Positive @Int
    it "A = Word, B = Positive" $ hedgehog $ check @Positive @Word
    it "A = Int,  B = Signed " $ hedgehog $ check @Signed @Int
    it "A = Word, B = Signed" $ hedgehog $ check @Signed @Word
