module Main (main) where

import Test.Hspec

import Board (Board (..), Cell (..), isSolved)

main :: IO ()
main = hspec $ do
  describe "Board.isSolved" $ do
    it "holds when no gems remain" $
      isSolved (Board 1 1 [Air]) `shouldBe` True
    it "fails while a gem remains" $
      isSolved (Board 2 1 [Gem, Air]) `shouldBe` False
