module Day01
  ( calculateHighestElfCalories
  , calculateHighestThreeElfCalories
  , puzzleOneInput
  ) where

import Import

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Prelude

calculateHighestElfCalories :: Text -> Int
calculateHighestElfCalories input =
  let elfCalories = toElfCalories input
  in
    fromJust $ maximumMay $ fmap sum elfCalories

calculateHighestThreeElfCalories :: Text -> Int
calculateHighestThreeElfCalories input =
  let calories = toElfCalories input
      elfCalories = fmap sum calories

      compareDescending a b =
        if a > b then
          LT
        else if a < b then
          GT
        else
          EQ
  in
    sum $ take 3 $ sortBy compareDescending elfCalories

toElfCalories :: Text -> [[Int]]
toElfCalories input =
  let inputLines = lines input
      elfCaloriesText = splitElem "" inputLines
      readText = Prelude.read . Text.unpack
  in
    (fmap . fmap) readText elfCaloriesText

puzzleOneInput :: IO Text
puzzleOneInput = Text.readFile "src/Day01/puzzle_one_input.txt"
