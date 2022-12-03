{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Day03Spec
  ( spec
  ) where

import TestImport

import qualified Day03

import qualified Prelude

-- input = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

spec :: Spec
spec = do
  describe "Day Three" $ do
    let input = [st|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|]
    describe "Puzzle One" $ do
      describe "Simple Input" $ do
        describe "Ruck Sacks" $ do
          let ruckSacks = Day03.ruckSacks input

          it "Parses first ruck sack correctly" $ do
            let controlFirstRuckSack = "vJrwpWtwJgWrhcsFMMfFFhFp"
                firstRuckSack = ruckSacks Prelude.!! 0

            firstRuckSack == controlFirstRuckSack

          it "Parses third ruck sack correctly" $ do
            let controlThirdRuckSack = "PmmdzqPrVvPwwTWBwg"
                thirdRuckSack = ruckSacks Prelude.!! 2

            thirdRuckSack == controlThirdRuckSack

          describe "Containers" $ do
            let ruckSack = ruckSacks Prelude.!! 0

                controlContainerLeft = "vJrwpWtwJgWr"
                controlContainerRight = "hcsFMMfFFhFp"

                containers = Day03.containers ruckSack

            it "Parses the left side correctly" $ do
              let containerLeft = containers Prelude.!! 0

              containerLeft `shouldBe` controlContainerLeft


            it "Parses the right side correctly" $ do
              let containerRight = containers Prelude.!! 1

              containerRight `shouldBe` controlContainerRight

            describe "Common Item Type" $ do
              let containerLeft = "vJrwpWtwJgWr"
                  containerRight = "hcsFMMfFFhFp"

              it "Finds common item type 'p'" $ do
                let commonItemType = Day03.commonItemType containerLeft containerRight

                commonItemType `shouldBe` 'p'

              describe "Priority" $ do
                it "'p' has priority of 16" $ do
                  let priority = Day03.priority 'p'

                  priority `shouldBe` 16

                it "'P' has priority of 42" $ do
                  let priority = Day03.priority 'P'

                  priority `shouldBe` 42

        describe "Calculation" $ do
          it "Priority sum is 157" $ do
            let prioritySum = Day03.prioritySum input

            prioritySum `shouldBe` 157

    describe "Puzzle Two" $ do
      describe "Groups of Three" $ do
        it "Groups items in groups of three" $ do
          let sample = [1..9] :: [Int]

              grouped = Day03.groupsOfThree sample

              expectedGroups = [[1..3], [4..6], [7..9]] :: [[Int]]

          grouped `shouldBe` expectedGroups

      describe "Simple Input" $ do
        describe "Elf Groups" $ do
          let groups = Day03.elfGroups input
              firstGroup = groups Prelude.!! 0

          it "Correctly finds the first group" $ do
            let controlFirstGroup = ["vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"]

            firstGroup `shouldBe` controlFirstGroup

          describe "Badges" $ do
            it "First groups badge is 'r'" $ do
              let badge = Day03.groupBadge firstGroup

              badge `shouldBe` 'r'

        describe "Calculation" $ do
          it "Badge sum is 70" $ do
            let badgeSum = Day03.badgeSum input

            badgeSum `shouldBe` 70
