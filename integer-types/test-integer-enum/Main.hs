{-# options_ghc -fno-warn-missing-signatures #-}

module Main (main) where

import Integer

import Data.Function (($))
import Data.List (take)
import Test.Hspec (describe, hspec, it, shouldBe)

main = hspec $ do
    describe "Positive" $ do
        describe "[a ..]" $ do
            it "counts upward" $
                take 3 [5 :: Positive ..] `shouldBe` [5, 6, 7]
            it "can start with 1" $
                take 3 [1 :: Positive ..] `shouldBe` [1, 2, 3]
        describe "[a .. b]" $ do
            it "counts upward" $
                [5 .. 8 :: Positive] `shouldBe` [5, 6, 7, 8]
            it "can start with 1" $
                [1 .. 5 :: Positive] `shouldBe` [1, 2, 3, 4, 5]
            it "does not count downward" $ do
                [8 .. 5 :: Positive] `shouldBe` []
                [8 .. 7 :: Positive] `shouldBe` []
            it "can return 1 item" $ do
                [3 .. 3 :: Positive] `shouldBe` [3]
                [1 .. 1 :: Positive] `shouldBe` [1]
        describe "[a, b ..]" $ do
            it "can count upward by 1" $ do
                take 5 [5, 6 :: Positive ..] `shouldBe` [5, 6, 7, 8, 9]
                take 5 [1, 2 :: Positive ..] `shouldBe` [1, 2, 3, 4, 5]
            it "can count downward by 1" $
                [5, 4 :: Positive ..] `shouldBe` [5, 4, 3, 2, 1]
            it "can count upward by 2" $ do
                take 5 [5, 7 :: Positive ..] `shouldBe` [5, 7, 9, 11, 13]
                take 5 [1, 3 :: Positive ..] `shouldBe` [1, 3, 5, 7, 9]
            it "can count downward by 2" $
                [9, 7 :: Positive ..] `shouldBe` [9, 7, 5, 3, 1]
            it "can count downward by 2 without exactly reaching its lower bound" $
                [8, 6 :: Positive ..] `shouldBe` [8, 6, 4, 2]
            it "can repeat 1 item indefinitely" $
                take 5 [4, 4 :: Positive ..] `shouldBe` [4, 4, 4, 4, 4]
        describe "[a, b .. c]" $ do
            it "can count upward by 1" $ do
                [5, 6 .. 9 :: Positive] `shouldBe` [5, 6, 7, 8, 9]
                [1, 2 .. 5 :: Positive] `shouldBe` [1, 2, 3, 4, 5]
            it "can count downward by 1" $
                [9, 8 .. 5 :: Positive] `shouldBe` [9, 8, 7, 6, 5]
            it "can count upward by 2" $ do
                [5, 7 .. 11 :: Positive] `shouldBe` [5, 7, 9, 11]
                [1, 3 .. 7 :: Positive] `shouldBe` [1, 3, 5, 7]
            it "can count upward without exactly reaching its upper bound" $
                [5, 7 .. 12 :: Positive] `shouldBe` [5, 7, 9, 11]
            it "can count downward by 2" $
                [11, 9 .. 5 :: Positive] `shouldBe` [11, 9, 7, 5]
            it "can count downward by 2 without exactly reaching its lower bound" $
                [11, 9 .. 4 :: Positive] `shouldBe` [11, 9, 7, 5]
            it "can count downward with a lower bound of 1" $ do
                [7, 5 .. 1 :: Positive] `shouldBe` [7, 5, 3, 1]
                [8, 6 .. 1 :: Positive] `shouldBe` [8, 6, 4, 2]
            it "can repeat 1 item indefinitely" $ do
                take 5 [4, 4 .. 9 :: Positive] `shouldBe` [4, 4, 4, 4, 4]
                take 5 [4, 4 .. 4 :: Positive] `shouldBe` [4, 4, 4, 4, 4]
            it "can return 1 item" $ do
                [4, 5 .. 4 :: Positive] `shouldBe` [4]
                [4, 3 .. 4 :: Positive] `shouldBe` [4]
            it "can return an empty list" $ do
                [4, 4 .. 3 :: Positive] `shouldBe` []
                [4, 5 .. 3 :: Positive] `shouldBe` []
                [5, 4 .. 6 :: Positive] `shouldBe` []
