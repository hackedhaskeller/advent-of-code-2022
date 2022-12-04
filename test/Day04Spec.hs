{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day04Spec
  ( spec
  , sampleInput
  ) where


import TestImport

import qualified Day04

import qualified Data.HashSet as Set
import qualified Prelude

sampleInput :: Text
sampleInput = [st|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|]

spec :: Spec
spec = do
  fdescribe "Day Four" $ do
    describe "Puzzle One" $ do
      describe "Sample Input" $ do
        describe "Parsing" $ do
          describe "Elf Pairs" $ do
            let elfPairs = Day04.elfPairs sampleInput
                firstElfPair = elfPairs Prelude.!! 0

            it "First pair is 2-4,6-8" $ do
              firstElfPair `shouldBe` "2-4,6-8"

            describe "Splitting" $ do
              let elves = Day04.elves firstElfPair
                  leftElf = elves Prelude.!! 0

              it "Left elf is 2-4" $ do
                leftElf `shouldBe` "2-4"

              describe "Expanding" $ do
                it "2-4 is 2, 3, 4" $ do
                  let controlExpansion = Set.fromList [2, 3, 4]
                      expansion = Day04.expand "2-4"

                  expansion `shouldBe` controlExpansion

          describe "Contain" $ do
            describe "Left Contains Right" $ do
              it "2-8 contains 3-7" $ do
                let leftGroup = Day04.expand "2-8"
                    rightGroup = Day04.expand "3-7"

                    contained = Day04.contains leftGroup rightGroup

                contained `shouldBe` True

            describe "Right Contains Left" $ do
              it "6-6 is contained by 4-6" $ do
                let leftGroup = Day04.expand "6-6"
                    rightGroup = Day04.expand "4-6"

                    contained = Day04.contains leftGroup rightGroup

                contained `shouldBe` True

          describe "Doesn't Contain" $ do
            it "2-3 does not contain 4-5" $ do
              let leftGroup = Day04.expand "2-3"
                  rightGroup = Day04.expand "4-5"

                  contained = Day04.contains leftGroup rightGroup

              contained `shouldBe` False

        describe "Calculation" $ do
          it "2 are contained" $ do
            let contained = Day04.countContained sampleInput

            contained `shouldBe` 2


    describe "Puzzle Two" $ do
      describe "Sample Input" $ do
        describe "Overlap" $ do
          describe "Contained" $ do
            it "2-8 overlaps with 3-7" $ do
              let leftGroup = Day04.expand "2-8"
                  rightGroup = Day04.expand "3-7"

                  overlaped = Day04.overlaps leftGroup rightGroup

              overlaped `shouldBe` True

          describe "Not Contained" $ do
            it "5-7 overlaps with 7-9" $ do
              let leftGroup = Day04.expand "5-7"
                  rightGroup = Day04.expand "7-9"

                  overlaped = Day04.overlaps leftGroup rightGroup

              overlaped `shouldBe` True

        describe "Doesn't Overlap" $ do
          it "2-4 doesn't overlap with 6-8" $ do
            let leftGroup = Day04.expand "2-4"
                rightGroup = Day04.expand "6-8"

                overlaped = Day04.overlaps leftGroup rightGroup

            overlaped `shouldBe` False

        describe "Calculation" $ do
          it "4 are overlapping" $ do
            let overlaped = Day04.countOverlapped sampleInput

            overlaped `shouldBe` 4
