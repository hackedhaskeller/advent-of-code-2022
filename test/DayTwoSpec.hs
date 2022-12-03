{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module DayTwoSpec
  ( spec
  ) where

import TestImport

import qualified DayTwo

spec :: Spec
spec = do
  describe "Day One" $ do
    let input = [st|A Y
B X
C Z|]

    describe "Puzzle One" $ do
      describe "Simple Input" $ do
        it "Calculates the points earned from the games to be 15" $ do
          let points = DayTwo.calculatePoints input

              expectedPoints = 15

          points `shouldBe` expectedPoints

    describe "Puzzle Two" $ do
      describe "Simple Input" $ do
        it "Calculates the points earned from the games to be 12" $ do
          let points = DayTwo.calculateActualStrategyPoints input

              expectedPoints = 12

          points `shouldBe` expectedPoints
