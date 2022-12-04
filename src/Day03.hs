module Day03
  ( -- | Puzzles
    prioritySum
  , badgeSum
  -- | Helpers
  , ruckSacks
  , containers
  , commonItemType
  , score
  , priority
  , groupsOfThree
  , elfGroups
  , groupBadge
  -- | Input
  , puzzleOneInput
  ) where


import Import

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

prioritySum :: Text -> Int
prioritySum input =
  let ruckSacks' = ruckSacks input
      containers' :: [[Text]]
      containers' = fmap containers ruckSacks'
      commonItems = fmap toCommonItem containers'
      priorities = fmap score commonItems
  in
    sum priorities
  where
    toCommonItem (left:right:[]) = commonItemType left right
    toCommonItem _ = error "Incorrect container structure"

ruckSacks :: Text -> [Text]
ruckSacks input = lines input

containers :: Text -> [Text]
containers ruckSack =
  let containerLength = length ruckSack `div` 2
      (left, right) = Text.splitAt containerLength ruckSack
  in
    [left, right]

commonItemType :: Text -> Text -> Char
commonItemType left right =
  fromJust $ Text.find (`Text.elem` left) right

priority :: Char -> Int
priority = score

itemScores :: Map.HashMap Char Int
itemScores = Map.fromList $ zip (join [['a'..'z'],['A'..'Z']]) [1..]

score :: Char -> Int
score score' =
  fromJust $ Map.lookup score' itemScores

badgeSum :: Text -> Int
badgeSum input =
  let groups = elfGroups input
      badges = fmap groupBadge groups
      scores = fmap score badges
  in
    sum scores

elfGroups :: Text -> [[Text]]
elfGroups input =
  let ruckSacks' = ruckSacks input
  in
    groupsOfThree ruckSacks'

groupsOfThree :: [a] -> [[a]]
groupsOfThree items =
  reverse $ grouper items []
  where
    grouper :: [a] -> [[a]] -> [[a]]
    grouper [] grouped = grouped
    grouper leftOver grouped =
      grouper (drop 3 leftOver) (take 3 leftOver : grouped)


groupBadge :: [Text] -> Char
groupBadge elfs =
  let sets = fmap (Set.fromList . Text.unpack) elfs
      commonChars = List.foldl1' Set.intersection sets
  in
    fromJust . headMay $ Set.toList commonChars

puzzleOneInput :: IO Text
puzzleOneInput = Text.readFile "src/Day03/puzzle_one_input.txt"
