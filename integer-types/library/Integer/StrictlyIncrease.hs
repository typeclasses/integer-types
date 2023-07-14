module Integer.StrictlyIncrease where

import Integer.Integer (Integer)
import Integer.Integer qualified as Integer
import Integer.Natural (Natural)
import Integer.Natural qualified as Natural
import Integer.Positive (Positive)
import Integer.Signed (Signed)
import Integer.Signed qualified as Signed
import Prelude qualified as Num (Num (..))

-- | Class of numbers that are closed under addition with 'Positive'
class StrictlyIncrease a where
  -- | Addition
  strictlyIncrease :: Positive -> a -> a

instance StrictlyIncrease Integer where
  strictlyIncrease = Integer.strictlyIncrease

instance StrictlyIncrease Signed where
  strictlyIncrease = Signed.strictlyIncrease

instance StrictlyIncrease Natural where
  strictlyIncrease = Natural.strictlyIncrease

instance StrictlyIncrease Positive where
  strictlyIncrease = (Num.+)
