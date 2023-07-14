module Main (main) where

import Control.DeepSeq (NFData, ($!!))
import Control.Exception (Exception, throw)
import Control.Exception qualified as Exception (ArithException (Underflow))
import Control.Monad.Catch qualified as Exception (MonadCatch, try)
import Data.Bool qualified as Bool
import Data.Either (Either (..))
import Data.Either qualified as Either
import Data.Int (Int)
import Data.List (take)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord qualified as Ord
import Data.Word (Word)
import Essentials
import Hedgehog qualified
import Integer
import Integer.Gen (GenFinite, GenIntegral)
import Integer.Gen qualified as Gen
import Integer.Natural qualified as Natural
import Integer.Positive qualified as Positive
import Integer.Signed qualified as Signed
import System.IO (IO)
import Test.Hspec (context, hspec, it, shouldBe)
import Test.Hspec.Hedgehog (evalMaybe, hedgehog, modifyMaxSuccess, (===))
import Prelude (Num, fromInteger, toInteger, ($!), (*), (+), (-))
import Prelude qualified as Bounded (Bounded (..))
import Prelude qualified as Num (fromInteger, toInteger)

main :: IO ()
main = hspec do
  context "addOne in A behaves the same as (+ 1) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Natural" $ hedgehog do
        x :: Natural <- Hedgehog.forAll Gen.integral
        toInteger (Natural.addOne x) === toInteger x + 1
      it "A = Positive" $ hedgehog do
        x :: Positive <- Hedgehog.forAll Gen.integral
        toInteger (Positive.addOne x) === toInteger x + 1
      it "A = Signed" $ hedgehog do
        x :: Signed <- Hedgehog.forAll Gen.integral
        toInteger (Signed.addOne x) === toInteger x + 1

  context "subtractOne in A behaves the same as (- 1) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      it "A = Positive" $ hedgehog do
        x :: Positive <- Hedgehog.forAll Gen.integral
        toInteger (Positive.subtractOne x) === toInteger x - 1
      it "A = Signed" $ hedgehog do
        x :: Signed <- Hedgehog.forAll Gen.integral
        toInteger (Signed.subtractOne x) === toInteger x - 1

  context "Closed Num operations op behaves the same in A as in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            GenIntegral a =>
            Monad m =>
            (forall b. Num b => b -> b -> b) ->
            Hedgehog.PropertyT m ()
          check o = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            x `o` y === fromInteger (toInteger x `o` toInteger y)

      it "op = (+), A = Positive" $ hedgehog $ check @Positive (+)
      it "op = (+), A = Signed" $ hedgehog $ check @Signed (+)
      it "op = (*), A = Positive" $ hedgehog $ check @Positive (*)
      it "op = (*), A = Signed" $ hedgehog $ check @Signed (*)

  context "subtract in A behaves the same as (-) in B" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, Subtraction a, Subtraction' b, Num b) =>
            (IntegerConvert a b, IntegerNarrow b a) =>
            (Eq b, Show b) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            (subtract x y :: b) === (convert x - convert y :: b)

      it "A = Natural,  B = Signed" $ hedgehog $ check @Natural @Signed
      it "A = Natural,  B = Integer" $ hedgehog $ check @Natural @Integer
      it "A = Positive, B = Signed" $ hedgehog $ check @Positive @Signed
      it "A = Positive, B = Integer" $ hedgehog $ check @Positive @Integer

  context "(-) in A behaves the same as (-) in Integer if the result is in A, undefined otherwise" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, Subtraction a, IntegerNarrow Integer a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            case narrow (toInteger x - toInteger y) :: Maybe a of
              Just z -> x - y === z
              Nothing -> do
                z <- Exception.try (pure $! x - y)
                z === Either.Left Exception.Underflow

      it "A = Positive" $ hedgehog $ check @Positive

  context "increase in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, Increase a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: Natural <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            toInteger (increase x y) === toInteger x + toInteger y

      it "A = Natural" $ hedgehog $ check @Natural
      it "A = Integer" $ hedgehog $ check @Integer
      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed

  context "strictlyIncrease in A behaves the same as (+) in Integer" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a m.
            (GenIntegral a, StrictlyIncrease a) =>
            Exception.MonadCatch m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: Positive <- Hedgehog.forAll Gen.integral
            y :: a <- Hedgehog.forAll Gen.integral
            toInteger (strictlyIncrease x y) === toInteger x + toInteger y

      it "A = Natural" $ hedgehog $ check @Natural
      it "A = Integer" $ hedgehog $ check @Integer
      it "A = Positive" $ hedgehog $ check @Positive
      it "A = Signed" $ hedgehog $ check @Signed

  context "convert (convert x) = x" do
    let check ::
          forall a b m.
          (GenIntegral a, IntegerEquiv a b) =>
          Monad m =>
          Hedgehog.PropertyT m ()
        check = do
          x :: a <- Hedgehog.forAll Gen.integral
          convert (convert x :: b) === x

    it "A = Integer, B = Signed" $ hedgehog $ check @Integer @Signed
    it "A = Signed,  B = Integer" $ hedgehog $ check @Signed @Integer

  context "narrow (convert x) = Just x" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, IntegerConvert a b, IntegerNarrow b a) =>
            Monad m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            narrow (convert x :: b) === Just x

      it "A = Natural,  B = Integer" $ hedgehog $ check @Natural @Integer
      it "A = Natural,  B = Signed" $ hedgehog $ check @Natural @Signed
      it "A = Positive, B = Integer" $ hedgehog $ check @Positive @Integer
      it "A = Positive, B = Signed" $ hedgehog $ check @Positive @Signed
      it "A = Positive, B = Natural" $ hedgehog $ check @Positive @Natural

  context "narrow x = (Just y | convert y = x) or Nothing" do
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            (GenIntegral a, BoundedBelow b) =>
            (IntegerConvert b a, IntegerNarrow a b) =>
            (Show b, Eq b) =>
            Monad m =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            let y :: Maybe b = narrow x
            if x Ord.>= convert (minBound @b)
              then do
                z <- evalMaybe y
                convert z === x
              else y === Nothing

      it "A = Integer, B = Natural" $ hedgehog $ check @Integer @Natural
      it "A = Signed,  B = Natural" $ hedgehog $ check @Signed @Natural
      it "A = Integer, B = Positive" $ hedgehog $ check @Integer @Positive
      it "A = Signed,  B = Positive" $ hedgehog $ check @Signed @Positive
      it "A = Natural, B = Positive" $ hedgehog $ check @Natural @Positive

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

  context "toFinite x = (Just y | fromInteger y = x) or Nothing" $
    modifyMaxSuccess (\_ -> 1000) do
      let check ::
            forall a b m.
            Monad m =>
            (ConvertWithFinite a, GenIntegral a, Show a) =>
            (Integer.Finite b, Eq b, Show b) =>
            Hedgehog.PropertyT m ()
          check = do
            x :: a <- Hedgehog.forAll Gen.integral
            let x' = Num.toInteger x
            let ok =
                  x' Ord.>= Num.toInteger (Bounded.minBound :: b)
                    Bool.&& x' Ord.<= Num.toInteger (Bounded.maxBound :: b)
            (Integer.toFinite x :: Maybe b)
              === if ok then Just (Num.fromInteger x') else Nothing

      it "A = Integer,  B = Int " $ hedgehog $ check @Integer @Int
      it "A = Integer,  B = Word" $ hedgehog $ check @Integer @Word
      it "A = Natural,  B = Int " $ hedgehog $ check @Natural @Int
      it "A = Natural,  B = Word" $ hedgehog $ check @Natural @Word
      it "A = Positive, B = Int " $ hedgehog $ check @Positive @Int
      it "A = Positive, B = Word" $ hedgehog $ check @Positive @Word
      it "A = Signed,   B = Int " $ hedgehog $ check @Signed @Int
      it "A = Signed,   B = Word" $ hedgehog $ check @Signed @Word

  context "fromFinite x = narrow (toInteger x)" do
    let check ::
          forall a b m.
          Monad m =>
          (ConvertWithFinite a, IntegerNarrow Integer a, Eq a, Show a) =>
          (Finite b, GenFinite b, Show b) =>
          Hedgehog.PropertyT m ()
        check = do
          x :: b <- Hedgehog.forAll Gen.finite
          (Integer.fromFinite x :: Maybe a) === Integer.narrow (Num.toInteger x)

    it "A = Int,  B = Integer " $ hedgehog $ check @Integer @Int
    it "A = Word, B = Integer" $ hedgehog $ check @Integer @Word
    it "A = Int,  B = Natural " $ hedgehog $ check @Natural @Int
    it "A = Word, B = Natural" $ hedgehog $ check @Natural @Word
    it "A = Int,  B = Positive " $ hedgehog $ check @Positive @Int
    it "A = Word, B = Positive" $ hedgehog $ check @Positive @Word
    it "A = Int,  B = Signed " $ hedgehog $ check @Signed @Int
    it "A = Word, B = Signed" $ hedgehog $ check @Signed @Word

  context "Enum @Positive" do
    let (~>) = shouldBe @[Positive]

    context "[a ..]" do
      it "counts upward" do
        take 3 [5 ..] ~> [5, 6, 7]
      it "can start with 1" do
        take 3 [1 ..] ~> [1, 2, 3]

    context "[a .. b]" do
      it "counts upward" do
        [5 .. 8] ~> [5, 6, 7, 8]
      it "can start with 1" do
        [1 .. 5] ~> [1, 2, 3, 4, 5]
      it "does not count downward" do
        [8 .. 5] ~> []
        [8 .. 7] ~> []
      it "can return 1 item" do
        [3 .. 3] ~> [3]
        [1 .. 1] ~> [1]

    context "[a, b ..]" do
      it "can count upward by 1" do
        take 5 [5, 6 ..] ~> [5, 6, 7, 8, 9]
        take 5 [1, 2 ..] ~> [1, 2, 3, 4, 5]
      it "can count downward by 1" do
        [5, 4 ..] ~> [5, 4, 3, 2, 1]
      it "can count upward by 2" do
        take 5 [5, 7 ..] ~> [5, 7, 9, 11, 13]
        take 5 [1, 3 ..] ~> [1, 3, 5, 7, 9]
      it "can count downward by 2" do
        [9, 7 ..] ~> [9, 7, 5, 3, 1]
      it "can count downward by 2 without exactly reaching its lower bound" do
        [8, 6 ..] ~> [8, 6, 4, 2]
      it "can repeat 1 item indefinitely" do
        take 5 [4, 4 ..] ~> [4, 4, 4, 4, 4]

    context "[a, b .. c]" do
      it "can count upward by 1" do
        [5, 6 .. 9] ~> [5, 6, 7, 8, 9]
        [1, 2 .. 5] ~> [1, 2, 3, 4, 5]
      it "can count downward by 1" do
        [9, 8 .. 5] ~> [9, 8, 7, 6, 5]
      it "can count upward by 2" do
        [5, 7 .. 11] ~> [5, 7, 9, 11]
        [1, 3 .. 7] ~> [1, 3, 5, 7]
      it "can count upward without exactly reaching its upper bound" do
        [5, 7 .. 12] ~> [5, 7, 9, 11]
      it "can count downward by 2" do
        [11, 9 .. 5] ~> [11, 9, 7, 5]
      it "can count downward by 2 without exactly reaching its lower bound" do
        [11, 9 .. 4] ~> [11, 9, 7, 5]
      it "can count downward with a lower bound of 1" do
        [7, 5 .. 1] ~> [7, 5, 3, 1]
        [8, 6 .. 1] ~> [8, 6, 4, 2]
      it "can repeat 1 item indefinitely" do
        take 5 [4, 4 .. 9] ~> [4, 4, 4, 4, 4]
        take 5 [4, 4 .. 4] ~> [4, 4, 4, 4, 4]
      it "can return 1 item" do
        [4, 5 .. 4] ~> [4]
        [4, 3 .. 4] ~> [4]
      it "can return an empty list" do
        [4, 4 .. 3] ~> []
        [4, 5 .. 3] ~> []
        [5, 4 .. 6] ~> []

  context "deepseq @Signed" do
    let (~>) = shouldBe @(Either X Signed)

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

  context "length" do
    it "Natural" do
      Natural.length "abc" `shouldBe` 3
    it "Positive" do
      Positive.length ('a' :| "bc") `shouldBe` 3

data X = X
  deriving stock (Eq, Show)

instance Exception X

force :: NFData a => Exception.MonadCatch m => a -> m (Either X a)
force x = Exception.try (pure $!! x)
