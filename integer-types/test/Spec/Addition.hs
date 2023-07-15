module Spec.Addition where

import Essentials
import Hedgehog qualified
import Integer (Positive, Signed)
import Integer.Gen (GenIntegral)
import Integer.Gen qualified as Gen
import Test.Hspec (Spec, context, it)
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess, (===))
import Prelude (fromInteger, toInteger, (+))

spec :: Spec
spec =
  context "(+) behaves the same in A as in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            GenIntegral a =>
            Monad m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            x + y === fromInteger (toInteger x + toInteger y)

      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed
