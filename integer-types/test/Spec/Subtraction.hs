module Spec.Subtraction where

import Control.Exception qualified as Exception (ArithException (Underflow))
import Control.Monad.Catch qualified as Exception (MonadCatch, try)
import Data.Either qualified as Either
import Essentials
import Hedgehog qualified
import Integer
  ( Integer,
    IntegerConvert (..),
    IntegerNarrow (..),
    Natural,
    Positive,
    Signed,
    Subtraction,
    Subtraction' (..),
  )
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude (Num, toInteger, ($!), (-))

spec :: Spec
spec = do
  context "subtract in A behaves the same as (-) in B" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, Subtraction a, Subtraction' b, Num b) =>
            (IntegerConvert a b, IntegerNarrow b a) =>
            (Eq b, Show b) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            (subtract x y :: b) === (convert x - convert y :: b)

      it "A = Natural,  B = Signed" $ hedgehog $ check @Natural @Signed
      it "A = Natural,  B = Integer" $ hedgehog $ check @Natural @Integer
      it "A = Positive, B = Signed" $ hedgehog $ check @Positive @Signed
      it "A = Positive, B = Integer" $ hedgehog $ check @Positive @Integer

  context "(-) in A behaves the same as (-) in Integer if the result is in A, undefined otherwise" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, Subtraction a, IntegerNarrow Integer a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            case narrow (toInteger x - toInteger y) :: Maybe a of
              Just z -> x - y === z
              Nothing -> do
                z <- Exception.try (pure $! x - y)
                z === Either.Left Exception.Underflow

      it "A = Positive" $ hedgehog $ check @Positive
