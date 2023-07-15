module Integer.AbsoluteDifference where

import Essentials
import Integer.Integer (Integer)
import Integer.Integer qualified as Integer
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Integer.Signed (Signed (Minus, NonZero, Plus, Zero))
import Integer.Signed qualified as Signed
import Prelude (fromInteger, (+), (-))
import Prelude qualified as Num (abs)

class AbsoluteDifference a b where
  absoluteDifference :: a -> b -> Natural

--

instance AbsoluteDifference Integer Integer where
  absoluteDifference a b = fromInteger $ Num.abs $ a - b

instance AbsoluteDifference Natural Natural where
  absoluteDifference a b = if a >= b then a - b else b - a

instance AbsoluteDifference Positive Positive where
  absoluteDifference a b =
    absoluteDifference
      (Positive.toNatural a)
      (Positive.toNatural b)

instance AbsoluteDifference Signed Signed where
  absoluteDifference Zero Zero = 0
  absoluteDifference Zero (NonZero _ x) = Positive.toNatural x
  absoluteDifference (NonZero _ x) Zero = Positive.toNatural x
  absoluteDifference (NonZero s1 x1) (NonZero s2 x2) =
    if s1 == s2
      then absoluteDifference x1 x2
      else Positive.toNatural (x1 + x2)

--

instance AbsoluteDifference Positive Natural where
  absoluteDifference p n = absoluteDifference (Positive.toNatural p) n

instance AbsoluteDifference Natural Positive where
  absoluteDifference n p = absoluteDifference p n

--

instance AbsoluteDifference Signed Natural where
  absoluteDifference Zero n = n
  absoluteDifference (Plus a) b = absoluteDifference a b
  absoluteDifference (Minus a) b = Positive.toNatural a + b

instance AbsoluteDifference Natural Signed where
  absoluteDifference n s = absoluteDifference s n

--

instance AbsoluteDifference Integer Natural where
  absoluteDifference i n = absoluteDifference (Integer.toSigned i) n

instance AbsoluteDifference Natural Integer where
  absoluteDifference n i = absoluteDifference i n

--

instance AbsoluteDifference Signed Positive where
  absoluteDifference Zero p = Positive.toNatural p
  absoluteDifference (Plus a) b = absoluteDifference a b
  absoluteDifference (Minus a) b = Positive.toNatural (a + b)

instance AbsoluteDifference Positive Signed where
  absoluteDifference p s = absoluteDifference s p

--

instance AbsoluteDifference Signed Integer where
  absoluteDifference s i = absoluteDifference (Signed.toInteger s) i

instance AbsoluteDifference Integer Signed where
  absoluteDifference i s = absoluteDifference s i

--

instance AbsoluteDifference Positive Integer where
  absoluteDifference p i = absoluteDifference (Positive.toInteger p) i

instance AbsoluteDifference Integer Positive where
  absoluteDifference i p = absoluteDifference p i
