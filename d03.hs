module Main where

import Text.Regex.Posix ((=~))

parse :: String -> [String]
parse input =
  let regexPattern = "mul\\([0-9]+,[0-9]+\\)"
      subs = (input =~ regexPattern)
   in map head subs

compute :: String -> Int
compute match =
  case match =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]] of
    [[_, a, b]] -> read a * read b

task1 :: String -> Int
task1 = sum . map compute . parse

parse2 :: String -> [String]
parse2 input =
  let regexPattern = "mul\\([0-9]+,[0-9]+\\)|don\\'t\\(\\)|do\\(\\)"
      subs = (input =~ regexPattern)
   in map head subs

task2 :: String -> Int
task2 = fst . foldl go (0, True) . parse2
  where
    go :: (Int, Bool) -> String -> (Int, Bool)
    go (acc, _) "don't()" = (acc, False)
    go (acc, _) "do()" = (acc, True)
    go (acc, True) s = (acc + compute s, True)
    go (acc, False) _ = (acc, False)

main :: IO ()
main = do
  input <- readFile "d03.txt"
  print $ task1 input
  print $ task2 input