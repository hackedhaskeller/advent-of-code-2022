{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day02Spec
  ( spec
  ) where

import TestImport

import qualified Day02

spec :: Spec
spec = do
  describe "Day Two" $ do
    let input = [st|A Y
B X
C Z|]

    describe "Puzzle One" $ do
      describe "Simple Input" $ do
        it "Calculates the points earned from the games to be 15" $ do
          let points = Day02.calculatePoints input

              expectedPoints = 15

          points `shouldBe` expectedPoints

    describe "Puzzle Two" $ do
      describe "Simple Input" $ do
        it "Calculates the points earned from the games to be 12" $ do
          let points = Day02.calculateActualStrategyPoints input

              expectedPoints = 12

          points `shouldBe` expectedPoints
