{-# language TemplateHaskell #-}
{-# options_ghc -fno-warn-missing-signatures #-}

module Main (main) where

import Integer

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Maybe (Maybe (..))
import Hedgehog (evalMaybe, (===))
import Integer.Gen (GenIntegral)
import Prelude (($!))
import Text.Show (Show)

import qualified Control.Exception as Exception (ArithException (Underflow))
import qualified Control.Monad.Catch as Exception (MonadCatch, try)
import qualified Data.Either as Either
import qualified Data.Ord as Ord
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified Integer.Gen as Gen
import qualified Prelude as Num (toInteger)

main = Hedgehog.defaultMain [Hedgehog.checkSequential $$(Hedgehog.discover)]

testLimit :: Hedgehog.TestLimit
testLimit = 1000

property = Hedgehog.withTests testLimit . Hedgehog.property

-- | Half of an isomorphism test: @review (view x)@ = @x@
checkIso :: forall a b m. (GenIntegral a, IntegerEquiv a b) =>
    Monad m => Hedgehog.PropertyT m ()
checkIso = do
    x :: a <- Hedgehog.forAll Gen.integral
    convert (convert x :: b) === x

-- | Half of a prism test: @preview (x review)@ = @Just@
checkConvert :: forall a b m.
    (GenIntegral a, IntegerConvert a b, IntegerNarrow b a) =>
    Monad m => Hedgehog.PropertyT m ()
checkConvert = do
    x :: a <- Hedgehog.forAll Gen.integral
    narrow (convert x :: b) === Just x

-- | Half of a prism test: @fmap review (preview x)@ = @Just x@
-- for @x@ in range, @Nothing@ otherwise
checkNarrow :: forall a b m. (GenIntegral a, BoundedBelow b) =>
    (IntegerConvert b a, IntegerNarrow a b) =>
    (Show b, Eq b) => Monad m => Hedgehog.PropertyT m ()
checkNarrow = do
    x :: a <- Hedgehog.forAll Gen.integral
    let y :: Maybe b = narrow x
    if x Ord.>= convert (minBound @b)
      then do
          z <- evalMaybe y
          convert z === x
      else y === Nothing

-- | Like 'checkNarrow @Integer', but tests the partial 'yolo'
-- function rather than the safe 'convert' function
checkYolo :: forall a m. (GenIntegral a, BoundedBelow a) =>
    Exception.MonadCatch m => Hedgehog.PropertyT m ()
checkYolo = do
    x :: Integer <- Hedgehog.forAll Gen.integral
    let y :: a = yolo x
    if x Ord.>= Num.toInteger (minBound @a)
      then yolo y === x
      else do
          z <- Exception.try (pure $! y)
          z === Either.Left Exception.Underflow

prop_iso_integer_signed = property $ checkIso @Integer @Signed
prop_iso_signed_integer = property $ checkIso @Signed @Integer

prop_prism_natural_integer  = property $ checkConvert @Natural  @Integer
prop_prism_integer_natural  = property $ checkNarrow  @Integer  @Natural
prop_prism_natural_signed   = property $ checkConvert @Natural  @Signed
prop_prism_signed_natural   = property $ checkNarrow  @Signed   @Natural
prop_prism_positive_integer = property $ checkConvert @Positive @Integer
prop_prism_integer_positive = property $ checkNarrow  @Integer  @Positive
prop_prism_positive_signed  = property $ checkConvert @Positive @Signed
prop_prism_signed_positive  = property $ checkNarrow  @Signed   @Positive
prop_prism_positive_natural = property $ checkConvert @Positive @Natural
prop_prism_natural_positive = property $ checkNarrow  @Natural  @Positive

prop_yolo_positive = property $ checkYolo @Positive
prop_yolo_natural  = property $ checkYolo @Natural
