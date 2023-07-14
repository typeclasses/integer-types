module Integer.Integer
  ( -- * Type
    Integer,

    -- * Conversion

    -- ** Positive
    toPositive,
    fromPositive,

    -- ** Natural
    toNatural,
    fromNatural,

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

    -- ** Increase
    increase,
    strictlyIncrease,
  )
where

import Data.Bool qualified as Bool
import Data.Int (Int)
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Integer.Natural qualified as Natural
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Integer.Signed (Signed (..))
import Integer.Signed qualified as Signed
import Numeric.Natural (Natural)
import Prelude (Integer)
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (Integral (..), Num (..))

toPositive :: Integer -> Maybe Positive
toPositive = Positive.fromInteger

fromPositive :: Positive -> Integer
fromPositive = Positive.toInteger

toNatural :: Integer -> Maybe Natural
toNatural = Natural.fromInteger

fromNatural :: Natural -> Integer
fromNatural = Natural.toInteger

toSigned :: Integer -> Signed
toSigned = Signed.fromInteger

fromSigned :: Signed -> Integer
fromSigned = Signed.toInteger

toInt :: Integer -> Maybe Int
toInt x = if ok then Just (Num.fromInteger x) else Nothing
  where
    ok =
      x Ord.>= Num.toInteger (Bounded.minBound :: Int)
        Bool.&& x Ord.<= Num.toInteger (Bounded.maxBound :: Int)

fromInt :: Int -> Integer
fromInt = Num.toInteger

toWord :: Integer -> Maybe Word
toWord x = if ok then Just (Num.fromInteger x) else Nothing
  where
    ok =
      x Ord.>= Num.toInteger (Bounded.minBound :: Word)
        Bool.&& x Ord.<= Num.toInteger (Bounded.maxBound :: Word)

fromWord :: Word -> Integer
fromWord = Num.toInteger

increase :: Natural -> Integer -> Integer
increase n = (Num.+) (fromNatural n)

strictlyIncrease :: Positive -> Integer -> Integer
strictlyIncrease n = (Num.+) (fromPositive n)
