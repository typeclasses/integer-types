{-# language Trustworthy #-}

module Integer.Natural
  (
    {- * Type -} Natural,
    {- * Subtraction -} subtract,
    {- * Conversion -}
    {- ** Positive -} toPositive, fromPositive,
    {- ** Integer -} toInteger, fromInteger,
    {- ** Signed -} toSigned, fromSigned,
    {- ** Int -} toInt, fromInt,
    {- ** Word -} toWord, fromWord,
    {- * One (1) -} one, addOne, subtractOne,
  )
  where

import Data.Function (($))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Word (Word)
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)

import qualified Data.Ord as Ord
import qualified Integer.Positive as Positive
import qualified Integer.Positive.Unsafe as Positive.Unsafe
import qualified Integer.Signed as Signed
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Num (Integral (..), Num (..))

toPositive :: Natural -> Maybe Positive.Unsafe.Positive
toPositive = Positive.fromNatural

fromPositive :: Positive.Unsafe.Positive -> Natural
fromPositive = Positive.toNatural

fromInteger :: Integer -> Maybe Natural
fromInteger x = if x Ord.>= 0 then Just (Num.fromInteger x) else Nothing

toInteger :: Natural -> Integer
toInteger = Num.toInteger

toSigned :: Natural -> Signed
toSigned = Signed.fromNatural

fromSigned :: Signed -> Maybe Natural
fromSigned = Signed.toNatural

toInt :: Natural -> Maybe Int
toInt x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.<= Num.toInteger (Bounded.maxBound :: Int)
    x' = Num.toInteger x

fromInt :: Int -> Maybe Natural
fromInt x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x Ord.>= 0
    x' = Num.toInteger x

toWord :: Natural -> Maybe Word
toWord x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.<= Num.toInteger (Bounded.maxBound :: Word)
    x' = Num.toInteger x

fromWord :: Word -> Natural
fromWord x = Num.fromInteger (Num.toInteger x)

subtract :: Natural -> Natural -> Signed
subtract a b = case Ord.compare a b of
    Ord.EQ -> Zero
    Ord.GT -> Plus  $ Positive.Unsafe.fromNatural $ (Num.-) a b
    Ord.LT -> Minus $ Positive.Unsafe.fromNatural $ (Num.-) b a

one :: Natural
one = 1

addOne :: Integer -> Integer
addOne = (Num.+ 1)

subtractOne :: Natural -> Maybe Signed
subtractOne x = case x of
    0 -> Nothing
    p -> Just (subtract p 1)
