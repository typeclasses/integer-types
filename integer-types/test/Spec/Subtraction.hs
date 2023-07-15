module Spec.Subtraction where

import Control.Exception (ArithException (Underflow))
import Control.Monad.Catch (MonadCatch, try)
import Data.Either qualified as Either
import Essentials
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
import Test.Hspec.Hedgehog
  ( PropertyT,
    forAll,
    hedgehog,
    modifyMaxSuccess,
    (===),
  )
import Prelude (Num, toInteger, ($!), (-))

spec :: Spec
spec = do
  context "subtract in A behaves the same as (-) in B" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Natural,  B = Signed" $ hedgehog $ checkSubtract @Natural @Signed
      it "A = Natural,  B = Integer" $ hedgehog $ checkSubtract @Natural @Integer
      it "A = Positive, B = Signed" $ hedgehog $ checkSubtract @Positive @Signed
      it "A = Positive, B = Integer" $ hedgehog $ checkSubtract @Positive @Integer

  context "(-) in A behaves the same as (-) in Integer if the result is in A, undefined otherwise" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Positive" $ hedgehog $ checkNumMinus @Positive

checkSubtract ::
  forall a b m.
  (GenIntegral a, Subtraction a, Subtraction' b, Num b) =>
  (IntegerConvert a b, IntegerNarrow b a) =>
  (Eq b, Show b) =>
  MonadCatch m =>
  PropertyT m ()
checkSubtract = do
  x :: a <- forAll Gen.integral
  y :: a <- forAll Gen.integral
  (subtract x y :: b) === (convert x - convert y :: b)

checkNumMinus ::
  forall a m.
  (GenIntegral a, Subtraction a, IntegerNarrow Integer a) =>
  MonadCatch m =>
  PropertyT m ()
checkNumMinus = do
  x :: a <- forAll Gen.integral
  y :: a <- forAll Gen.integral
  case narrow (toInteger x - toInteger y) :: Maybe a of
    Just z -> x - y === z
    Nothing -> do
      z <- try (pure $! x - y)
      z === Either.Left Underflow
