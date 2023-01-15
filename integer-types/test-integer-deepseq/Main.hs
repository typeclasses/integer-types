{-# options_ghc -fno-warn-missing-signatures #-}

module Main (main) where

import Integer

import Control.Applicative (pure)
import Control.DeepSeq (NFData, ($!!))
import Control.Exception (Exception, throw)
import Data.Either (Either (..))
import Data.Eq (Eq)
import Data.Function (($))
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Show (Show)

import qualified Control.Monad.Catch as Exception (MonadCatch, try)

data X = X
    deriving (Eq, Show)

instance Exception X

force :: NFData a => Exception.MonadCatch m => a -> m (Either X a)
force x = Exception.try (pure $!! x)

main = hspec $ do
    describe "Signed" $ do
        describe "deepseq" $ do
            it "can succeed" $ do
                x <- force (NonZero MinusSign 5)
                x `shouldBe` Right (-5)
            it "can force an error" $ do
                x <- force (throw X :: Signed)
                x `shouldBe` Left X
            it "can force an error in sign" $ do
                x <- force (NonZero (throw X) 5)
                x `shouldBe` Left X
            it "can force an error in magnitude" $ do
                x <- force (NonZero MinusSign (throw X))
                x `shouldBe` Left X
