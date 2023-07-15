module Spec.Deepseq where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception (Exception, throw)
import Control.Monad.Catch qualified as Exception (MonadCatch, try)
import Data.Either (Either (..))
import Essentials
import Integer (Sign (MinusSign), Signed (NonZero))
import Test.Hspec (Expectation, Spec, context, it, shouldBe)

spec :: Spec
spec =
  context "deepseq @Signed" do
    it "can succeed" do
      x <- force (NonZero MinusSign 5)
      x ~> Right (-5)
    it "can force an error" do
      x <- force (throw X)
      x ~> Left X
    it "can force an error in sign" do
      x <- force (NonZero (throw X) 5)
      x ~> Left X
    it "can force an error in magnitude" do
      x <- force (NonZero MinusSign (throw X))
      x ~> Left X

(~>) :: Either X Signed -> Either X Signed -> Expectation
(~>) = shouldBe @(Either X Signed)

data X = X
  deriving stock (Eq, Show)

instance Exception X

force :: NFData a => Exception.MonadCatch m => a -> m (Either X a)
force x = Exception.try (pure $!! x)
