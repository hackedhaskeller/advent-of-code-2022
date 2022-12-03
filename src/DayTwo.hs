module DayTwo
  ( calculatePoints
  , puzzleOneInput
  , calculateActualStrategyPoints
  ) where

import Import

-- Note: This code was written when _really_ tired ... probably not good

import qualified Data.Text.IO as Text
import qualified Prelude

calculatePoints :: Text -> Int
calculatePoints input =
  let games = parseToGames input
      scores = fmap score games
  in
    sum scores
  where
    score :: (PaperRockScissor, PaperRockScissor) -> Int
    score (Paper, Paper) = (choicePoints Paper) + draw
    score (Paper, Rock) = (choicePoints Paper) + win
    score (Paper, Scissor) = (choicePoints Paper) + loss
    score (Rock, Paper) = (choicePoints Rock) + loss
    score (Rock, Rock) = (choicePoints Rock) + draw
    score (Rock, Scissor) = (choicePoints Rock) + win
    score (Scissor, Paper) = (choicePoints Scissor) + win
    score (Scissor, Rock) = (choicePoints Scissor) + loss
    score (Scissor, Scissor) = (choicePoints Scissor) + draw

calculateActualStrategyPoints :: Text -> Int
calculateActualStrategyPoints input =
  let strategies = parseToStrategy input
      games = fmap strategyToGame strategies
      scores = fmap score games
  in
    sum scores
  where
    score :: (PaperRockScissor, PaperRockScissor) -> Int
    score (Paper, Paper) = (choicePoints Paper) + draw
    score (Paper, Rock) = (choicePoints Paper) + win
    score (Paper, Scissor) = (choicePoints Paper) + loss
    score (Rock, Paper) = (choicePoints Rock) + loss
    score (Rock, Rock) = (choicePoints Rock) + draw
    score (Rock, Scissor) = (choicePoints Rock) + win
    score (Scissor, Paper) = (choicePoints Scissor) + win
    score (Scissor, Rock) = (choicePoints Scissor) + loss
    score (Scissor, Scissor) = (choicePoints Scissor) + draw

data PaperRockScissor
  = Paper
  | Rock
  | Scissor
  deriving Show

parseToGames :: Text -> [(PaperRockScissor, PaperRockScissor)]
parseToGames input =
  let games = lines input
  in
    foldl' (\games' next ->
              let throws = words next

                  opponentThrow = convertOpponentThrow $ throws Prelude.!! 0
                  myThrow = convertMyThrow $ throws Prelude.!! 1
              in
                (myThrow, opponentThrow) : games'
           ) [] games

data Strategy
  = Lose
  | Draw
  | Win

parseToStrategy :: Text -> [(Strategy, PaperRockScissor)]
parseToStrategy input =
  let strategies = lines input
  in
    foldl' (\strategies' next ->
              let throws = words next

                  opponentThrow = convertOpponentThrow $ throws Prelude.!! 0
                  myStrategy = convertMyStrategy $ throws Prelude.!! 1
              in
                (myStrategy, opponentThrow) : strategies'
           ) [] strategies

strategyToGame :: (Strategy, PaperRockScissor) -> (PaperRockScissor, PaperRockScissor)
strategyToGame (Lose, Paper) = (Rock, Paper)
strategyToGame (Draw, Paper) = (Paper, Paper)
strategyToGame (Win, Paper) = (Scissor, Paper)
strategyToGame (Lose, Rock) = (Scissor, Rock)
strategyToGame (Draw, Rock) = (Rock, Rock)
strategyToGame (Win, Rock) = (Paper, Rock)
strategyToGame (Lose, Scissor) = (Paper, Scissor)
strategyToGame (Draw, Scissor) = (Scissor, Scissor)
strategyToGame (Win, Scissor) = (Rock, Scissor)

convertOpponentThrow :: Text -> PaperRockScissor
convertOpponentThrow "A" = Rock
convertOpponentThrow "B" = Paper
convertOpponentThrow "C" = Scissor
convertOpponentThrow _ = error "this shouldn't happen"

convertMyThrow :: Text -> PaperRockScissor
convertMyThrow "X" = Rock
convertMyThrow "Y" = Paper
convertMyThrow "Z" = Scissor
convertMyThrow _ = error "this shouldn't happen"

convertMyStrategy :: Text -> Strategy
convertMyStrategy "X" = Lose
convertMyStrategy "Y" = Draw
convertMyStrategy "Z" = Win
convertMyStrategy _ = error "this shouldn't happen"


choicePoints :: PaperRockScissor -> Int
choicePoints Paper = 2
choicePoints Rock = 1
choicePoints Scissor = 3

draw :: Int
draw = 3

win :: Int
win = 6

loss :: Int
loss = 0

puzzleOneInput :: IO Text
puzzleOneInput = Text.readFile "src/DayTwo/puzzle_one_input.txt"
