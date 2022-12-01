module Aoc.Day1b

import Data.List1
import Data.String
import System.File.ReadWrite

sumCalories : List String -> Integer
sumCalories inventory = case sequence $ map parseInteger inventory of
                            Just cs => sum cs
                            Nothing => ?handle_nothing

solve : String -> String
solve = show . sum . take 3 . reverse . sort . forget . map sumCalories . split (== "") . lines 

main : IO ()
main = do f <- readFile "input.txt"
          case f of
              Left err => ?handle_err
              Right str => putStrLn $ solve str