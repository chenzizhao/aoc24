module Main where

import Data.List (sort)

parse :: String -> ([Int], [Int])
parse s =
  let ll = (map read . words) s
      l1 = [x | (x, i) <- zip ll [0 ..], odd i]
      l2 = [x | (x, i) <- zip ll [0 ..], even i]
   in (l1, l2)

task1 :: String -> Int
task1 s =
  let (l1, l2) = parse s
      l1' = sort l1
      l2' = sort l2
   in sum $ zipWith (\a b -> abs (a - b)) l1' l2'

count :: [Int] -> Int -> Int
count [] _ = 0
count (x : xs) n = if x == n then 1 + count xs n else count xs n

task2 :: String -> Int
task2 s =
  let (l1, l2) = parse s
   in sum $ map (\x -> count l2 x * x) l1

main :: IO ()
main = do
  input <- readFile "d01.txt"
  print $ task1 input
  print $ task2 input