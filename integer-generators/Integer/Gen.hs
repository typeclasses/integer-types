module Integer.Gen
  (
    GenIntegral (integral),
    GenFinite (finite),
    astronomical,
  )
  where

import Control.Applicative (pure, (<*>))
import Data.Function (id, ($))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.Word (Word)
import Integer (BoundedBelow (..), Integer, Natural, Positive, Sign (..),
                Signed (..))
import Text.Show (Show)

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as Bounded (Bounded (..))
import qualified Prelude as Num (Integral (..), Num (..), (+), (^))

---

class (Num.Integral a, Show a) => GenIntegral a
  where
    -- | Generators for 'Integer', 'Natural', 'Positive',
    -- or 'Signed' selected from one of three methods:
    --
    -- * small numbers (magnitude less than ten)
    -- * large numbers (well in excess of 64-bit)
    -- * numbers at or around a bound of 'Int' or 'Word'
    integral :: Hedgehog.Gen a

instance GenIntegral Integer  where integral = integer
instance GenIntegral Natural  where integral = boundedBelow
instance GenIntegral Positive where integral = boundedBelow
instance GenIntegral Signed   where integral = signed

---

class (Num.Integral a, Bounded.Bounded a, Show a) => GenFinite a
  where
    finite :: Hedgehog.Gen a

instance GenFinite Int where finite = defaultFinite

instance GenFinite Word where finite = defaultFinite

defaultFinite :: (Num.Integral a, Bounded.Bounded a) => Hedgehog.Gen a
defaultFinite = Gen.choice
    [ Gen.integral $ Range.linear Bounded.minBound Bounded.maxBound
    , Gen.integral $ Range.linear Bounded.maxBound Bounded.minBound
    ]

---

smol :: Num.Integral a => a
smol = 10

astronomical :: Num.Integral a => a
astronomical = 2 Num.^ (99 :: Integer)

bigRange :: Num.Integral a => Range.Range a
bigRange = Range.exponential smol astronomical

---

integer :: Hedgehog.Gen Integer
integer = Gen.choice [smolInteger, nearFiniteBoundInteger, bigInteger]

smolInteger :: Hedgehog.Gen Integer
smolInteger = Gen.integral $ Range.linearFrom 0 (Num.negate smol) smol

bigInteger :: Hedgehog.Gen Integer
bigInteger = Gen.element [id, Num.negate] <*> Gen.integral bigRange

nearFiniteBoundInteger :: Hedgehog.Gen Integer
nearFiniteBoundInteger = Gen.element [id, Num.negate] <*> nearPositiveFiniteBound

---

boundedBelow :: forall a. (BoundedBelow a, Num.Integral a) => Hedgehog.Gen a
boundedBelow = Gen.choice [smolBoundedBelow, nearPositiveFiniteBound, bigBoundedBelow]

smolBoundedBelow :: forall a. (BoundedBelow a, Num.Integral a) => Hedgehog.Gen a
smolBoundedBelow = fmap Num.fromInteger $ Gen.integral $ Range.linear (Num.toInteger $ minBound @a) smol

bigBoundedBelow :: forall a. (BoundedBelow a, Num.Integral a) => Hedgehog.Gen a
bigBoundedBelow = fmap Num.fromInteger $ Gen.integral bigRange

nearPositiveFiniteBound :: forall a. Num.Integral a => Hedgehog.Gen a
nearPositiveFiniteBound = fmap Num.fromInteger $
    pure (Num.+)
    <*> Gen.element
      [ Num.toInteger (Bounded.maxBound :: Int)
      , Num.toInteger (Bounded.maxBound :: Word)
      ]
    <*> smolInteger

---

signed :: Hedgehog.Gen Signed
signed = Gen.choice [smolSigned, nearFiniteBoundSigned, bigSigned]

smolSigned :: Hedgehog.Gen Signed
smolSigned = Gen.frequency
    [ (,) 1 $ pure Zero
    , (,) 9 $ pure NonZero <*> sign <*> smolBoundedBelow
    ]

bigSigned :: Hedgehog.Gen Signed
bigSigned = pure NonZero <*> sign <*> bigBoundedBelow

nearFiniteBoundSigned :: Hedgehog.Gen Signed
nearFiniteBoundSigned = pure NonZero <*> sign <*> nearPositiveFiniteBound

---

sign :: Hedgehog.Gen Sign
sign = Gen.element [PlusSign, MinusSign]
