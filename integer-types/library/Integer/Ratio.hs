module Integer.Ratio
  ( -- * Type
    Ratio,

    -- * Construction
    (%),

    -- * Fields
    numerator,
    denominator,
  )
where

import Essentials
import Integer.NonzeroMagnitudeAffineTraversal (NonzeroMagnitudeAffineTraversal)
import Integer.NonzeroMagnitudeAffineTraversal qualified as NonzeroMagnitude
import Integer.Positive (Positive)
import Integer.Positive qualified as Positive
import Prelude qualified as Integral (Integral (..))

data Ratio a = (:%)
  { numerator :: a,
    denominator :: Positive
  }

(%) :: NonzeroMagnitudeAffineTraversal a => a -> Positive -> Ratio a
x % y = case NonzeroMagnitude.preview x of
  Nothing -> x :% 1
  Just m ->
    let d = Positive.greatestCommonFactor m y
     in NonzeroMagnitude.set (Integral.quot m d) x :% Integral.quot y d
