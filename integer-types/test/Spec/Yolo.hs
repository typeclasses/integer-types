module Spec.Yolo where

import Control.Exception qualified as Exception (ArithException (Underflow))
import Control.Monad.Catch qualified as Exception (MonadCatch, try)
import Data.Either qualified as Either
import Data.Ord qualified as Ord
import Essentials
import Hedgehog qualified
import Integer
  ( BoundedBelow (..),
    Integer,
    Natural,
    Positive,
    yolo,
  )
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog, (===))
import Prelude (($!))
import Prelude qualified as Num (toInteger)

spec :: Spec
spec =
  context "yolo (yolo x) = x, if Integer x is in range of A" do
    let check ::
          forall a m.
          (GenIntegral a, BoundedBelow a) =>
          Exception.MonadCatch m =>
          Hedgehog.PropertyT m ()
        check = do
          x :: Integer <- Hedgehog.forAll Gen.integral
          let y :: a = yolo x
          if x Ord.>= Num.toInteger (minBound @a)
            then yolo y === x
            else do
              z <- Exception.try (pure $! y)
              z === Either.Left Exception.Underflow

    it "A = Positive" $ hedgehog $ check @Positive
    it "A = Natural " $ hedgehog $ check @Natural
