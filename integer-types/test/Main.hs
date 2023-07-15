module Main (main) where

import Spec.AbsoluteDifference qualified
import Spec.AddOne qualified
import Spec.Addition qualified
import Spec.Conversion qualified
import Spec.Deepseq qualified
import Spec.Enum qualified
import Spec.FiniteConversion qualified
import Spec.Increase qualified
import Spec.Length qualified
import Spec.Multiplication qualified
import Spec.SubtractOne qualified
import Spec.Subtraction qualified
import Spec.Yolo qualified
import System.IO (IO)
import Test.Hspec (hspec)

main :: IO ()
main = hspec do
  Spec.AbsoluteDifference.spec
  Spec.Addition.spec
  Spec.AddOne.spec
  Spec.Conversion.spec
  Spec.Deepseq.spec
  Spec.Enum.spec
  Spec.FiniteConversion.spec
  Spec.Increase.spec
  Spec.Length.spec
  Spec.Multiplication.spec
  Spec.Subtraction.spec
  Spec.SubtractOne.spec
  Spec.Yolo.spec
