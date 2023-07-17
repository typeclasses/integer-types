module Integer.Positive
  ( -- * Type
    Positive,

    -- * Conversion

    -- ** Natural
    toNatural,
    fromNatural,

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
    increase,

    -- ** Division
    divide,
    quotient,
    remainder,
    greatestCommonFactor,

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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Integer.Positive.Unsafe (Positive, addOne, increase, one, toInteger, toNatural)
import Integer.Positive.Unsafe qualified as Unsafe
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (Integral (..), Num (..))

fromInteger :: Integer -> Maybe Positive
fromInteger x = if x Ord.> 0 then Just (Unsafe.fromInteger x) else Nothing

fromNatural :: Natural -> Maybe Positive
fromNatural x = case x of 0 -> Nothing; _ -> Just (Unsafe.fromNatural x)

toInt :: Positive -> Maybe Int
toInt x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.<= Num.toInteger (Bounded.maxBound :: Int)
    x' = Num.toInteger x

fromInt :: Int -> Maybe Positive
fromInt x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.>= 1
    x' = Num.toInteger x

toWord :: Positive -> Maybe Word
toWord x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.<= Num.toInteger (Bounded.maxBound :: Word)
    x' = Num.toInteger x

fromWord :: Word -> Maybe Positive
fromWord x = if ok then Just (Num.fromInteger x') else Nothing
  where
    ok = x' Ord.>= 1
    x' = Num.toInteger x

subtract :: Positive -> Positive -> Signed
subtract a b = case Ord.compare a b of
  Ord.EQ -> Zero
  Ord.GT -> Plus $ Unsafe.subtract a b
  Ord.LT -> Minus $ Unsafe.subtract b a

subtractOne :: Positive -> Natural
subtractOne x = toNatural x Num.- 1

toSigned :: Positive -> Signed
toSigned = Plus

fromSigned :: Signed -> Maybe Positive
fromSigned (Plus x) = Just x
fromSigned _ = Nothing

length :: NonEmpty a -> Positive
length (_ :| xs) = List.foldl' (\x _ -> x Num.+ 1) 1 xs

-- | 'quotient' and 'remainder'
divide :: Positive -> Positive -> (Natural, Natural)
divide a b = Num.quotRem (toNatural a) (toNatural b)

quotient :: Positive -> Positive -> Natural
quotient a b = Num.quot (toNatural a) (toNatural b)

remainder :: Positive -> Positive -> Natural
remainder a b = Num.rem (toNatural a) (toNatural b)

greatestCommonFactor :: Positive -> Positive -> Positive
greatestCommonFactor a b =
  case fromNatural (remainder a b) of
    Nothing -> a
    Just c -> greatestCommonFactor b c
