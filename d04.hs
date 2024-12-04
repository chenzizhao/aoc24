module Main where

import Data.List (isPrefixOf)

parse :: String -> [[Char]]
parse = lines

span' :: [[Char]] -> (Int, Int) -> String -> (Int, Int) -> Bool
span' chars coord target dir =
  let inBound :: (Int, Int) -> Bool
      inBound (x, y) = x >= 0 && y >= 0 && x < length chars && y < length (chars !! x)
      indexGrid :: (Int, Int) -> Char
      indexGrid (x, y) = chars !! x !! y
      move :: (Int, Int) -> (Int, Int) -> (Int, Int)
      move (x, y) (dx, dy) = (x + dx, y + dy)
      slice = map indexGrid $ takeWhile inBound $ iterate (`move` dir) coord
   in target `isPrefixOf` slice

numValidSpans :: [[Char]] -> String -> (Int, Int) -> Int
numValidSpans chars target coord =
  let dirs = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
   in length $ filter (span' chars coord target) dirs

task1 :: String -> Int
task1 s =
  let target = "XMAS"
      chars = parse s
      startIdx =
        [ (x, y)
          | x <- [0 .. length chars - 1],
            y <- [0 .. length (chars !! x) - 1],
            chars !! x !! y == head target
        ]
   in sum $ map (numValidSpans chars target) startIdx

crossMatch :: [[Char]] -> String -> (Int, Int) -> Bool
crossMatch grid target (x, y)
  | x - 1 < 0 || y - 1 < 0 || x + 1 >= length grid || y + 1 >= length (grid !! x) = False
  | otherwise = slice' (1, 1) && slice' (1, -1)
  where
    slice :: (Int, Int) -> String
    slice (dx, dy) = [grid !! (x + m * dx) !! (y + m * dy) | m <- [-1, 0, 1]]
    slice' :: (Int, Int) -> Bool
    slice' dir = target == slice dir || target == reverse (slice dir)

task2 :: String -> Int
task2 s =
  let target = "MAS"
      chars = parse s
      startIdx =
        [ (x, y)
          | x <- [0 .. length chars - 1],
            y <- [0 .. length (chars !! x) - 1],
            chars !! x !! y == target !! 1
        ]
   in length $ filter (crossMatch chars target) startIdx

main :: IO ()
main = do
  input <- readFile "d04.txt"
  print $ task1 input
  print $ task2 input