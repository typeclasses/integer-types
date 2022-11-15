module Integer.BoundedBelow where

import Numeric.Natural (Natural)

class BoundedBelow a where
    minBound :: a

instance BoundedBelow Natural where
    minBound = 0


