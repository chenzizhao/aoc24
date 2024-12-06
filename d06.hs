module Main where

import Data.List (nub)

parse :: String -> [[Bool]]
parse = map (map (/= '#')) . lines -- true is open

findStartCoord :: String -> (Int, Int)
findStartCoord s =
  let grid :: [[Char]] = lines s
   in head
        [ (x, y)
          | x <- [0 .. length grid - 1],
            y <- [0 .. length (head grid) - 1],
            grid !! x !! y == '^'
        ]

-- cos sin
turnRight :: (Int, Int) -> (Int, Int)
turnRight (0, 1) = (1, 0)
turnRight (1, 0) = (0, -1)
turnRight (0, -1) = (-1, 0)
turnRight (-1, 0) = (0, 1)

move :: [[Bool]] -> (Bool, (Int, Int), (Int, Int)) -> (Bool, (Int, Int), (Int, Int))
move grid (_, (x, y), (dx, dy)) =
  let x' = x + dx
      y' = y + dy
      outOfBounds = x' < 0 || y' < 0 || x' >= length grid || y' >= length (head grid)
      isOpen = grid !! x' !! y'
   in if outOfBounds
        then (True, (x, y), (dx, dy))
        else
          if isOpen
            then (False, (x', y'), (dx, dy))
            else (False, (x, y), turnRight (dx, dy))

task1 :: String -> Int
task1 s =
  let grid = parse s
      startCoord = findStartCoord s
      startDir = (-1, 0)
      traces = iterate (move grid) (False, startCoord, startDir)
      traces' = takeWhile (\(done, _, _) -> not done) traces
      positions = nub $ map (\(_, p, _) -> p) traces'
   in length positions

causeLoop :: (Int, Int) -> [[Bool]] -> Bool
causeLoop startCoord grid =
  let startDir = (-1, 0)
      traces = iterate (move grid) (False, startCoord, startDir)
      traces' = takeWhile (\(done, _, _) -> not done) traces
      traces'' = take (length grid * length (head grid)) traces'
      poses = map (\(_, p, d) -> (p, d)) traces''
      hasDuplicates = length poses /= length (nub poses)
   in hasDuplicates

rewriteGrid :: [[Bool]] -> (Int, Int) -> [[Bool]]
rewriteGrid grid (x, y) =
  [ if rowIdx == x
      then
        [ if colIdx == y then False else val
          | (colIdx, val) <- zip [0 ..] row
        ]
      else row
    | (rowIdx, row) <- zip [0 ..] grid
  ]

task2 :: String -> Int
task2 s =
  let grid = parse s
      startCoord = findStartCoord s

      -- find openCoords
      startDir = (-1, 0)
      traces = iterate (move grid) (False, startCoord, startDir)
      traces' = takeWhile (\(done, _, _) -> not done) traces
      traces'' = take (length grid * length (head grid)) traces'
      coords = map (\(_, c, _) -> c) traces''
      openCoords = nub $ filter (/= startCoord)  coords

      altGrids = map (rewriteGrid grid) openCoords
      altGrids' = filter (causeLoop startCoord) altGrids
   in length altGrids'

main :: IO ()
main = do
  s <- readFile "d06.txt"
  print $ task1 s
  print $ task2 s