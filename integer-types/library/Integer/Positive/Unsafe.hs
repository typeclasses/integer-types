-- | This module is unsafe not merely in the sense that it contains partial
-- functions, but moreover than it is capable of constructing the invalid
-- 'Positive' value @'FromNatural' 0@ representing zero, which is not positive.
-- When a function has "checked" in its name, this indicates that it is partial but
-- will never construct an invalid 'Positive'.
module Integer.Positive.Unsafe
  ( -- * Type
    Positive (FromNatural),

    -- * Conversion

    -- ** Natural
    toNatural,
    fromNatural,
    fromNaturalChecked,

    -- ** Integer
    toInteger,
    fromInteger,
    fromIntegerChecked,

    -- ** Int
    toInt,
    fromInt,
    fromIntChecked,

    -- * Arithmetic

    -- ** Subtraction
    subtract,
    subtractChecked,

    -- ** Increase
    increase,

    -- ** One (1)
    one,
    addOne,
    subtractOne,
    subtractOneChecked,
  )
where

import Control.DeepSeq qualified as DeepSeq
import Control.Exception qualified as Exception
import Control.Monad.Fail (fail)
import Data.Bits qualified as Bits
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Essentials
import Integer.BoundedBelow (BoundedBelow)
import Integer.BoundedBelow qualified as BoundedBelow
import Numeric.Natural (Natural)
import Text.Read qualified as Read
import Text.Show qualified as Show
import Prelude (Int, Integer, Integral, Num, Read, Real)
import Prelude qualified as Enum (Enum (..))
import Prelude qualified as Num
  ( Integral (..),
    Num (..),
    Real (..),
    fromIntegral,
  )

newtype Positive = FromNatural {toNatural :: Natural}
  deriving newtype (Eq, Ord, Hashable)

instance DeepSeq.NFData Positive where rnf (FromNatural x) = DeepSeq.rnf x

fromNatural :: Natural -> Positive
fromNatural = FromNatural

fromNaturalChecked :: Natural -> Positive
fromNaturalChecked x = case x of 0 -> Exception.throw Exception.Underflow; _ -> fromNatural x

toInteger :: Positive -> Integer
toInteger = Num.toInteger . toNatural

fromInteger :: Integer -> Positive
fromInteger = fromNatural . Num.fromInteger

fromIntegerChecked :: Integer -> Positive
fromIntegerChecked x = if x Ord.>= 1 then fromInteger x else Exception.throw Exception.Underflow

add :: Positive -> Positive -> Positive
add a b = fromNatural (toNatural a Num.+ toNatural b)

subtract :: Positive -> Positive -> Positive
subtract a b = fromNatural (toNatural a Num.- toNatural b)

subtractChecked :: Positive -> Positive -> Positive
subtractChecked a b = if a Ord.> b then subtract a b else Exception.throw Exception.Underflow

multiply :: Positive -> Positive -> Positive
multiply a b = fromNatural (toNatural a Num.* toNatural b)

one :: Positive
one = fromNatural 1

addOne :: Positive -> Positive
addOne = fromNatural . (Num.+ 1) . toNatural

subtractOne :: Positive -> Positive
subtractOne = fromNatural . (Num.- 1) . toNatural

subtractOneChecked :: Positive -> Positive
subtractOneChecked x = case x of 1 -> Exception.throw Exception.Underflow; _ -> subtractOne x

increase :: Natural -> Positive -> Positive
increase n = fromNatural . (Num.+ n) . toNatural

toInt :: Positive -> Int
toInt = Num.fromIntegral . toNatural

toIntChecked :: Positive -> Int
toIntChecked = Maybe.fromMaybe (Exception.throw Exception.Overflow) . Bits.toIntegralSized . toNatural

fromInt :: Int -> Positive
fromInt = fromNatural . Num.fromIntegral

fromIntChecked :: Int -> Positive
fromIntChecked x = case Num.signum x of 1 -> fromInt x; _ -> Exception.throw Exception.Underflow

enumFrom :: Positive -> [Positive]
enumFrom = List.map fromNatural . Enum.enumFrom . toNatural

enumFromTo :: Positive -> Positive -> [Positive]
enumFromTo a b = List.map fromNatural $ Enum.enumFromTo (toNatural a) (toNatural b)

enumFromThen :: Positive -> Positive -> [Positive]
enumFromThen a b = if a Ord.< b then ascending else descending
  where
    ascending = List.map fromNatural $ Enum.enumFromThen (toNatural a) (toNatural b)
    descending =
      List.map fromInteger $
        List.takeWhile (Ord.>= 1) $
          Enum.enumFromThen (toInteger a) (toInteger b)

enumFromThenTo :: Positive -> Positive -> Positive -> [Positive]
enumFromThenTo a b c = if a Ord.< b then ascending else descending
  where
    ascending = List.map fromNatural $ Enum.enumFromThenTo (toNatural a) (toNatural b) (toNatural c)
    descending =
      List.map fromInteger $
        List.takeWhile (Ord.>= 1) $
          Enum.enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

type Div a = a -> a -> (a, a)

divisionOp :: Div Natural -> Div Positive
divisionOp o a b =
  let (q, r) = o (toNatural a) (toNatural b)
   in (fromNaturalChecked q, fromNaturalChecked r)

instance BoundedBelow Positive where
  minBound = 1

instance Num Positive where
  abs = id
  negate = \_ -> Exception.throw Exception.Underflow
  signum = \_ -> fromNatural 1
  fromInteger = fromIntegerChecked
  (+) = add
  (*) = multiply
  (-) = subtractChecked

instance Enum Positive where
  succ = addOne
  pred = subtractOneChecked

  fromEnum = toIntChecked
  toEnum = fromIntChecked

  enumFrom = enumFrom
  enumFromTo = enumFromTo
  enumFromThen = enumFromThen
  enumFromThenTo = enumFromThenTo

instance Real Positive where
  toRational = Num.toRational . toInteger

instance Integral Positive where
  toInteger = toInteger
  quotRem = divisionOp Num.quotRem
  divMod = divisionOp Num.divMod

instance Show Positive where
  show = Show.show . toNatural
  showsPrec i = Show.showsPrec i . toNatural

instance Read Positive where
  readsPrec i = do
    xs <- Read.readsPrec @Natural i
    pure $
      xs & Maybe.mapMaybe \case
        (0, _) -> Nothing
        (n, s) -> Just (fromNatural n, s)
  readPrec = do
    n <- Read.readPrec @Natural
    if n == 0 then fail "0" else pure $ fromNatural n
