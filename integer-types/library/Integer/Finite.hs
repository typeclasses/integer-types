module Integer.Finite where

import Data.Int (Int)
import Data.Maybe qualified as Maybe
import Data.Word (Word)
import Essentials
import Integer.Integer (Integer)
import Integer.Integer qualified as Integer
import Integer.Natural (Natural)
import Integer.Natural qualified as Natural
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Integer.Signed (Signed)
import Integer.Signed qualified as Signed
import Prelude (Integral)

class ConvertWithFinite a where
  toWord :: a -> Maybe Word
  fromWord :: Word -> Maybe a
  toInt :: a -> Maybe Int
  fromInt :: Int -> Maybe a

instance ConvertWithFinite Natural where
  toWord = Natural.toWord
  fromWord = Maybe.Just . Natural.fromWord
  toInt = Natural.toInt
  fromInt = Natural.fromInt

instance ConvertWithFinite Positive where
  toWord = Positive.toWord
  fromWord = Positive.fromWord
  toInt = Positive.toInt
  fromInt = Positive.fromInt

instance ConvertWithFinite Integer where
  toWord = Integer.toWord
  fromWord = Maybe.Just . Integer.fromWord
  toInt = Integer.toInt
  fromInt = Maybe.Just . Integer.fromInt

instance ConvertWithFinite Signed where
  toWord = Signed.toWord
  fromWord = Maybe.Just . Signed.fromWord
  toInt = Signed.toInt
  fromInt = Maybe.Just . Signed.fromInt

class (Bounded b, Integral b) => Finite b where
  toFinite :: ConvertWithFinite a => a -> Maybe b
  fromFinite :: ConvertWithFinite a => b -> Maybe a

instance Finite Int where
  toFinite = toInt
  fromFinite = fromInt

instance Finite Word where
  toFinite = toWord
  fromFinite = fromWord
