{-# language TemplateHaskell #-}
{-# options_ghc -fno-warn-missing-signatures #-}

module Main (main) where

import Integer

import Control.Applicative (pure)
import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Maybe (Maybe (Just, Nothing))
import Hedgehog ((===))
import Integer.Gen (GenIntegral)
import Prelude (Num, fromInteger, toInteger, ($!), (*), (+), (-))
import Text.Show (Show)

import qualified Control.Exception as Exception (ArithException (Underflow))
import qualified Control.Monad.Catch as Exception (MonadCatch, try)
import qualified Data.Either as Either
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified Integer.Gen as Gen

main = Hedgehog.defaultMain [Hedgehog.checkSequential $$(Hedgehog.discover)]

testLimit :: Hedgehog.TestLimit
testLimit = 1000

property = Hedgehog.withTests testLimit . Hedgehog.property

-- | Assert that a closed binary 'Num' operation behaves the same
-- on a type as it does when applied via conversion with 'Integer'
checkNumOp :: forall a m. GenIntegral a => Monad m =>
    (forall b. Num b => b -> b -> b) -> Hedgehog.PropertyT m ()
checkNumOp o = do
    x :: a <- Hedgehog.forAll Gen.integral
    y :: a <- Hedgehog.forAll Gen.integral
    x `o` y === fromInteger (toInteger x `o` toInteger y)

-- | Assert that 'subtract' in @a@ gives the same result
-- as '(-)' via @b@.
checkSubtract :: forall a b m.
    (GenIntegral a, Subtraction a, Subtraction' b, Num b) =>
    (IntegerConvert a b, IntegerNarrow b a) =>
    (Eq b, Show b) =>
    Exception.MonadCatch m => Hedgehog.PropertyT m ()
checkSubtract = do
    x :: a <- Hedgehog.forAll Gen.integral
    y :: a <- Hedgehog.forAll Gen.integral
    (subtract x y :: b) === (convert x - convert y :: b)

-- | Assert that '(-)' in @a@ gives the same result as
-- '(-)' via @Integer@ if the result is within the range
-- of @a@, and is undefined otherwise
checkPartialSubtract :: forall a m.
    (GenIntegral a, Subtraction a, IntegerNarrow Integer a) =>
    Exception.MonadCatch m => Hedgehog.PropertyT m ()
checkPartialSubtract = do
    x :: a <- Hedgehog.forAll Gen.integral
    y :: a <- Hedgehog.forAll Gen.integral
    case narrow (toInteger x - toInteger y) :: Maybe a of
        Just z -> x - y === z
        Nothing -> do
            z <- Exception.try (pure $! x - y)
            z === Either.Left Exception.Underflow

prop_add_positive      = property $ checkNumOp @Positive (+)
prop_add_signed        = property $ checkNumOp @Signed   (+)
prop_multiply_positive = property $ checkNumOp @Positive (*)
prop_multiply_signed   = property $ checkNumOp @Signed   (*)

prop_subtract_natural_signed   = property $ checkSubtract @Natural  @Signed
prop_subtract_natural_integer  = property $ checkSubtract @Natural  @Integer
prop_subtract_positive_signed  = property $ checkSubtract @Positive @Signed
prop_subtract_positive_integer = property $ checkSubtract @Positive @Integer

prop_partial_subtract_positive = property $ checkPartialSubtract @Positive
