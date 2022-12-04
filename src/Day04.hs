module Day04
  ( -- | Puzzles
    countContained
  , countOverlapped
  -- | Helpers
  , elfPairs
  , elves
  , expand
  , contains
  , overlaps
  , puzzleOneInput
  ) where

import Import hiding (intersection)

-- ## Remove me - Casey, Sun Dec 04 2022
-- import qualified Debug.Trace as Trace

import qualified Data.Ix as Ix
import qualified Data.HashSet as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Prelude

countContained :: Text -> Int
countContained input =
  let pairs = elfPairs input
      elves' = fmap elves pairs
      contained = fmap toContained elves'
  in
    length $ filter id contained
  where
    toContained (left:right:[]) =
      let expandedLeft = expand left
          expandedRight = expand right
      in
        contains expandedLeft expandedRight
    toContained _ = error "incorrect formation of elves"

countOverlapped :: Text -> Int
countOverlapped input =
  let pairs = elfPairs input
      elves' = fmap elves pairs
      overlaped = fmap toOverlaped elves'
  in
    length $ filter id overlaped
  where
    toOverlaped (left:right:[]) =
      let expandedLeft = expand left
          expandedRight = expand right
      in
        overlaps expandedLeft expandedRight
    toOverlaped _ = error "incorrect formation of elves"

elfPairs :: Text -> [Text]
elfPairs input = lines input

elves :: Text -> [Text]
elves elfPair =
  let separator = ','
  in
    Text.split (==separator) elfPair

expand :: Text -> Set.HashSet Int
expand sectionsCompact =
  let separator = '-'

      numbers :: [Int]
      numbers = catMaybes $ fmap readMay $ Text.split (==separator) sectionsCompact

      firstNumber = numbers Prelude.!! 0
      secondNumber = numbers Prelude.!! 1

  in
    Set.fromList $ Ix.range (firstNumber, secondNumber)

contains :: Set.HashSet Int -> Set.HashSet Int -> Bool
contains left right =
  let intersection = Set.intersection left right
  in
    (left == intersection) || (right == intersection)

overlaps :: Set.HashSet Int -> Set.HashSet Int -> Bool
overlaps left right =
  let intersection = Set.intersection left right
  in
    not $ Set.null intersection

puzzleOneInput :: IO Text
puzzleOneInput = Text.readFile "src/Day04/puzzle_one_input.txt"
