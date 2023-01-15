module Integer.Finite where

import Data.Function ((.))
import Data.Int (Int)
import Data.Maybe (Maybe)
import Data.Word (Word)
import Integer.Integer (Integer)
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Signed (Signed)
import Prelude (Bounded, Integral)

import qualified Data.Maybe as Maybe
import qualified Integer.Integer as Integer
import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed

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
