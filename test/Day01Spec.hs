{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day01Spec
  ( spec
  ) where

import TestImport

import qualified Day01

spec :: Spec
spec = do
  describe "Day One" $ do
    let input = [st|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|]

    describe "Puzzle One" $ do
      describe "Simple Input" $ do
        it "Calculates that the Elf with the most calories has 24000" $ do
          let highestElfCalories = Day01.calculateHighestElfCalories input

              expectedHighestElfCalories = 24000

          highestElfCalories `shouldBe` expectedHighestElfCalories

    describe "Puzzle Two" $ do
      describe "Simple Input" $ do
        it "Calculates the highest three calories are 45000" $ do
          let highestThree = Day01.calculateHighestThreeElfCalories input

              expectedHighestThree = 45000

          highestThree `shouldBe` expectedHighestThree
