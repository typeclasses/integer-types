{-# language TemplateHaskell #-}
{-# options_ghc -fno-warn-missing-signatures #-}

module Main (main) where

import Integer

import Control.Monad (Monad)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Word (Word)
import Hedgehog ((===))
import Integer.Gen (GenFinite, GenIntegral)
import Text.Show (Show)

import qualified Data.Bool as Bool
import qualified Data.Ord as Ord
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified Integer.Gen as Gen
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Num (fromInteger, toInteger)

main = Hedgehog.defaultMain [Hedgehog.checkSequential $$(Hedgehog.discover)]

testLimit :: Hedgehog.TestLimit
testLimit = 1000

property = Hedgehog.withTests testLimit . Hedgehog.property

checkToFinite :: forall a b m. Monad m =>
    (ConvertWithFinite a, GenIntegral a, Show a) =>
    (Integer.Finite b, Eq b, Show b) =>
    Hedgehog.PropertyT m ()
checkToFinite = do
    x :: a <- Hedgehog.forAll Gen.integral
    let x' = Num.toInteger x
    let ok = x' Ord.>= Num.toInteger (Bounded.minBound :: b) Bool.&&
             x' Ord.<= Num.toInteger (Bounded.maxBound :: b)
    (Integer.toFinite x :: Maybe b) ===
        if ok then Just (Num.fromInteger x') else Nothing

checkFromFinite :: forall a b m. Monad m =>
    (ConvertWithFinite a, IntegerNarrow Integer a, Eq a, Show a) =>
    (Finite b, GenFinite b, Show b) =>
    Hedgehog.PropertyT m ()
checkFromFinite = do
    x :: b <- Hedgehog.forAll Gen.finite
    (Integer.fromFinite x :: Maybe a) === Integer.narrow (Num.toInteger x)

prop_convert_integer_int   = property $ checkToFinite   @Integer @Int
prop_convert_int_integer   = property $ checkFromFinite @Integer @Int
prop_convert_integer_word  = property $ checkToFinite   @Integer @Word
prop_convert_word_integer  = property $ checkFromFinite @Integer @Word

prop_convert_natural_int   = property $ checkToFinite   @Natural @Int
prop_convert_int_natural   = property $ checkFromFinite @Natural @Int
prop_convert_natural_word  = property $ checkToFinite   @Natural @Word
prop_convert_word_natural  = property $ checkFromFinite @Natural @Word

prop_convert_positive_int  = property $ checkToFinite   @Positive @Int
prop_convert_int_positive  = property $ checkFromFinite @Positive @Int
prop_convert_positive_word = property $ checkToFinite   @Positive @Word
prop_convert_word_positive = property $ checkFromFinite @Positive @Word

prop_convert_signed_int    = property $ checkToFinite   @Signed @Int
prop_convert_int_signed    = property $ checkFromFinite @Signed @Int
prop_convert_signed_word   = property $ checkToFinite   @Signed @Word
prop_convert_word_signed   = property $ checkFromFinite @Signed @Word
