module Aoc.Day2b

import Data.String
import System.File.ReadWrite

%default total

data RpsChoice
  = Rock
  | Paper
  | Scissors

data RpsResult
  = Win
  | Loss
  | Draw

choiceScore : RpsChoice -> Integer
choiceScore Rock = 1
choiceScore Paper = 2
choiceScore Scissors = 3

resultScore : RpsResult -> Integer
resultScore Win = 6
resultScore Draw = 3
resultScore Loss = 0

roundScore : RpsChoice -> RpsResult -> Integer
roundScore c r = choiceScore c + resultScore r

choiceForResult : RpsChoice -> RpsResult -> RpsChoice
choiceForResult Rock Win = Paper
choiceForResult Rock Draw = Rock
choiceForResult Rock Loss = Scissors
choiceForResult Paper Win = Scissors
choiceForResult Paper Draw = Paper
choiceForResult Paper Loss = Rock
choiceForResult Scissors Win = Rock
choiceForResult Scissors Draw = Scissors
choiceForResult Scissors Loss = Paper

parseOpponentChar : Char -> Maybe RpsChoice
parseOpponentChar 'A' = Just Rock
parseOpponentChar 'B' = Just Paper
parseOpponentChar 'C' = Just Scissors
parseOpponentChar _ = Nothing

parseYourResult : Char -> Maybe RpsResult
parseYourResult 'X' = Just Loss
parseYourResult 'Y' = Just Draw
parseYourResult 'Z' = Just Win
parseYourResult _ = Nothing

parseLine : List Char -> Maybe (RpsResult, RpsChoice)
parseLine [p1, ' ', p2] = do
    yourResult <- parseYourResult p2
    opponentsChoice <- parseOpponentChar p1
    pure (yourResult, opponentsChoice)
parseLine _ = Nothing

lineScore : List Char -> Maybe Integer
lineScore l = do
    (result, opponentChoice) <- parseLine l
    let yourChoice = choiceForResult opponentChoice result
    pure $ roundScore yourChoice result

solve : String -> String
solve input = case (sequence $ map lineScore $ map unpack $ lines input) of
    Just a => show $ sum a
    Nothing => "Error in input"

partial
main : IO ()
main = do f <- readFile "input.txt"
          case f of
              Left err => ?handle_err
              Right str => putStrLn $ solve str