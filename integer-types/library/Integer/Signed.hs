module Integer.Signed
  (
    {- * Type -} Signed (Zero, NonZero, Plus, Minus, NotPlus, NotMinus),
    {- * Conversion -}
    {- ** Integer -} toInteger, fromInteger,
    {- ** Natural -} toNatural, fromNatural,
    {- ** Positive -} toPositive, fromPositive,
    {- ** Int -} toInt, fromInt,
    {- ** Word -} toWord, fromWord,
  )
  where

import Essentials

import Data.Hashable (Hashable (hashWithSalt))
import Data.Int (Int)
import Data.Word (Word)
import Integer.Positive.Unsafe (Positive)
import Integer.Sign (Sign (..))
import Numeric.Natural (Natural)
import Prelude (Integer, Integral, Num, Real, seq)

import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Integer.Positive.Unsafe as Positive.Unsafe
import qualified Integer.Sign as Sign
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Enum (Enum (..))
import qualified Prelude as Num (Integral (..), Num (..), Real (..))
import qualified Text.Show as Show

data Signed = Zero | NonZero Sign Positive
    deriving stock (Eq)

instance Ord Signed where
    compare Zero Zero = Ord.EQ

    compare Zero (Minus _) = Ord.GT
    compare Zero (Plus _ ) = Ord.LT
    compare (Minus _) Zero = Ord.LT
    compare (Plus  _) Zero = Ord.GT

    compare (Plus  _) (Minus _) = Ord.GT
    compare (Minus _) (Plus  _) = Ord.LT
    compare (Plus  a) (Plus  b) = Ord.compare a b
    compare (Minus a) (Minus b) = Ord.compare b a

instance DeepSeq.NFData Signed where
    rnf Zero          = ()
    rnf (NonZero a b) = a `seq` b `seq` ()

instance Hashable Signed where
    hashWithSalt s Zero      = s `hashWithSalt` ( 0 :: Int)
    hashWithSalt s (Plus  x) = s `hashWithSalt` ( 1 :: Int) `hashWithSalt` x
    hashWithSalt s (Minus x) = s `hashWithSalt` (-1 :: Int) `hashWithSalt` x

pattern Minus :: Positive -> Signed
pattern Minus x = NonZero MinusSign x
pattern Plus :: Positive -> Signed

pattern Plus x = NonZero PlusSign x

-- | A 'Signed' that is either zero or positive
pattern NotMinus :: Natural -> Signed
pattern NotMinus x <- (toNatural -> Just x)
  where NotMinus = fromNatural

-- | A 'Signed' that is either zero or negative;
-- the 'Natural' gives the magnitude of the negative
pattern NotPlus :: Natural -> Signed
pattern NotPlus x <- ((toNatural . negate) -> Just x)
  where NotPlus = negate . fromNatural

{-# complete Zero, Minus, Plus #-}
{-# complete Plus, NotPlus #-}
{-# complete Minus, NotMinus #-}

fromPositive :: Positive -> Signed
fromPositive = Plus

toPositive :: Signed -> Maybe Positive
toPositive (Plus x) = Just x
toPositive _        = Nothing

fromNatural :: Natural -> Signed
fromNatural 0 = Zero
fromNatural x = Plus $ Positive.Unsafe.fromNatural x

toNatural :: Signed -> Maybe Natural
toNatural (Minus _) = Nothing
toNatural Zero      = Just 0
toNatural (Plus x)  = Just (Positive.Unsafe.toNatural x)

add :: Signed -> Signed -> Signed
add Zero x = x
add x Zero = x
add (NonZero sa a) (NonZero sb b) = case (sa, sb) of
    (PlusSign, PlusSign)   -> Plus  $ a Num.+ b
    (MinusSign, MinusSign) -> Minus $ a Num.+ b

    (MinusSign, PlusSign) -> case Ord.compare a b of
        Ord.EQ -> Zero
        Ord.LT -> Plus  $ Positive.Unsafe.subtract b a
        Ord.GT -> Minus $ Positive.Unsafe.subtract a b

    (PlusSign, MinusSign) -> case Ord.compare a b of
        Ord.EQ -> Zero
        Ord.LT -> Minus $ Positive.Unsafe.subtract b a
        Ord.GT -> Plus  $ Positive.Unsafe.subtract a b

negate :: Signed -> Signed
negate Zero          = Zero
negate (NonZero s x) = NonZero (Sign.negate s) x

multiply :: Signed -> Signed -> Signed
multiply Zero _ = Zero
multiply _ Zero = Zero
multiply (NonZero sa a) (NonZero sb b) =
    NonZero (Sign.multiply sa sb) (a Num.* b)

abs :: Signed -> Signed
abs Zero = Zero
abs x@(NonZero s p) = case s of
    PlusSign  -> x
    MinusSign -> NonZero PlusSign p

signum :: Signed -> Signed
signum Zero          = Zero
signum (NonZero s _) = NonZero s Positive.Unsafe.one

fromInteger :: Integer -> Signed
fromInteger x = case Ord.compare x 0 of
    Ord.EQ -> Zero
    Ord.LT -> Minus $ Positive.Unsafe.fromInteger $ Num.abs x
    Ord.GT -> Plus  $ Positive.Unsafe.fromInteger x

toInteger :: Signed -> Integer
toInteger Zero      = 0
toInteger (Plus x)  = Positive.Unsafe.toInteger x
toInteger (Minus x) = Num.negate $ Positive.Unsafe.toInteger x

toInt :: Signed -> Maybe Int
toInt x = case x of
    Zero -> Just 0
    Plus p -> if ok then Just (Num.fromInteger i) else Nothing
      where
        ok = i Ord.<= Num.toInteger (Bounded.maxBound :: Int)
        i = Positive.Unsafe.toInteger p
    Minus p -> if ok then Just (Num.fromInteger i) else Nothing
      where
        ok = i Ord.>= Num.toInteger (Bounded.minBound :: Int)
        i = Num.negate (Positive.Unsafe.toInteger p)

fromInt :: Int -> Signed
fromInt x = case Ord.compare x 0 of
    Ord.EQ -> Zero
    Ord.GT -> Plus $ Positive.Unsafe.fromInt x
    Ord.LT -> Minus $ Positive.Unsafe.fromInteger $ Num.negate $ Num.toInteger x

toWord :: Signed -> Maybe Word
toWord x = case x of
    Zero -> Just 0
    Plus p -> if ok then Just (Num.fromInteger i) else Nothing
      where
        ok = i Ord.<= Num.toInteger (Bounded.maxBound :: Word)
        i = Positive.Unsafe.toInteger p
    Minus _ -> Nothing

fromWord :: Word -> Signed
fromWord x = case x of
    0 -> Zero
    _ -> Plus $ Positive.Unsafe.fromInteger (Num.toInteger x)

type Div a = a -> a -> (a, a)

divisionOp :: Div Integer -> Div Signed
divisionOp o a b =
    let (q, r) = o (toInteger a) (toInteger b)
    in (fromInteger q, fromInteger r)

instance Num Signed
  where
    (+) = add
    (*) = multiply
    negate = negate
    abs = abs
    signum = signum
    fromInteger = fromInteger

instance Enum Signed
  where
    pred = fromInteger . Enum.pred . toInteger
    succ = fromInteger . Enum.succ . toInteger

    toEnum = fromInteger . Enum.toEnum
    fromEnum = Enum.fromEnum . toInteger

    enumFrom a = List.map fromInteger $ Enum.enumFrom (toInteger a)
    enumFromTo a b = List.map fromInteger $ Enum.enumFromTo (toInteger a) (toInteger b)
    enumFromThen a b = List.map fromInteger $ Enum.enumFromThen (toInteger a) (toInteger b)
    enumFromThenTo a b c = List.map fromInteger $ Enum.enumFromThenTo (toInteger a) (toInteger b) (toInteger c)

instance Real Signed
  where
    toRational = Num.toRational . toInteger

instance Integral Signed
  where
    toInteger = toInteger
    quotRem = divisionOp Num.quotRem
    divMod = divisionOp Num.divMod

instance Show Signed
  where
    show = Show.show . Num.toInteger
    showsPrec i = Show.showsPrec i . Num.toInteger
