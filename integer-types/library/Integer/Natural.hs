module Integer.Natural
  ( -- * Type
    Natural,

    -- * Conversion

    -- ** Positive
    toPositive,
    fromPositive,

    -- ** Integer
    toInteger,
    fromInteger,

    -- ** Signed
    toSigned,
    fromSigned,

    -- ** Int
    toInt,
    fromInt,

    -- ** Word
    toWord,
    fromWord,

    -- * Arithmetic

    -- ** Subtraction
    subtract,

    -- ** Increase
    strictlyIncrease,

    -- ** One (1)
    one,
    addOne,
    subtractOne,

    -- * List
    length,
  )
where

import Data.Int (Int)
import Data.List qualified as List
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Integer.Positive qualified as Positive
import Integer.Positive.Unsafe (Positive)
import Integer.Positive.Unsafe qualified as Positive.Unsafe
import Integer.Signed (Signed (..))
import Integer.Signed qualified as Signed
import Numeric.Natural (Natural)
import Prelude (Integer)
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (Integral (..), Num (..))

toPositive :: Natural -> Maybe Positive
toPositive = Positive.fromNatural

fromPositive :: Positive -> Natural
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
  Ord.GT -> Plus $ Positive.Unsafe.fromNatural $ (Num.-) a b
  Ord.LT -> Minus $ Positive.Unsafe.fromNatural $ (Num.-) b a

one :: Natural
one = 1

addOne :: Natural -> Positive
addOne x = Positive.Unsafe.fromNatural (x Num.+ 1)

subtractOne :: Natural -> Maybe Signed
subtractOne x = case x of
  0 -> Nothing
  p -> Just (subtract p 1)

length :: [a] -> Natural
length = List.foldl' (\x _ -> x Num.+ 1) 0

strictlyIncrease :: Positive -> Natural -> Natural
strictlyIncrease p n = Positive.toNatural p Num.+ n
