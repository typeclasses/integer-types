module Spec.Enum where

import Data.List (take)
import Integer (Positive)
import Test.Hspec (Spec, context, it, shouldBe)

spec :: Spec
spec =
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
