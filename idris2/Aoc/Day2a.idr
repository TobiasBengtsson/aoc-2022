module Aoc.Day2a

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

roundResult : RpsChoice -> RpsChoice -> RpsResult
roundResult Rock Rock = Draw
roundResult Rock Paper = Loss
roundResult Rock Scissors = Win
roundResult Paper Rock = Win
roundResult Paper Paper = Draw
roundResult Paper Scissors = Loss
roundResult Scissors Rock = Loss
roundResult Scissors Paper = Win
roundResult Scissors Scissors = Draw

parseOpponentChar : Char -> Maybe RpsChoice
parseOpponentChar 'A' = Just Rock
parseOpponentChar 'B' = Just Paper
parseOpponentChar 'C' = Just Scissors
parseOpponentChar _ = Nothing

parseYourChar : Char -> Maybe RpsChoice
parseYourChar 'X' = Just Rock
parseYourChar 'Y' = Just Paper
parseYourChar 'Z' = Just Scissors
parseYourChar _ = Nothing

parseLine : List Char -> Maybe (RpsChoice, RpsChoice)
parseLine [p1, ' ', p2] = do
    yourChoice <- parseYourChar p2
    opponentsChoice <- parseOpponentChar p1
    pure (yourChoice, opponentsChoice)
parseLine _ = Nothing

lineScore : List Char -> Maybe Integer
lineScore l = do
    choices <- parseLine l
    let result = (uncurry roundResult) choices
    pure $ roundScore (fst choices) result

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