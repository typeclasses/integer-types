module Integer.Increase where

import Integer.Integer (Integer)
import Integer.Integer qualified as Integer
import Integer.Natural (Natural)
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Integer.Signed (Signed)
import Integer.Signed qualified as Signed
import Prelude qualified as Num (Num (..))

-- | Class of numbers are closed under addition with 'Natural'
class Increase a where
  -- | Addition
  increase :: Natural -> a -> a

instance Increase Integer where
  increase = Integer.increase

instance Increase Signed where
  increase = Signed.increase

instance Increase Natural where
  increase = (Num.+)

instance Increase Positive where
  increase = Positive.increase
