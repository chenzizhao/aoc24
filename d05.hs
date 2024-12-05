module Main where

import Data.Graph qualified as G
import Data.List (elemIndex, sortBy)

parse :: String -> ([(Int, Int)], [[Int]])
parse s =
  let (a, b) = span (/= "") $ lines s

      procPairs :: String -> (Int, Int)
      procPairs s =
        let (a, b) = span (/= '|') s
         in (read a, read $ tail b)

      procLists :: String -> [Int]
      procLists s = map read $ words $ map (\c -> if c == ',' then ' ' else c) s
   in (map procPairs a, map procLists (tail b))

validUpdate :: [(Int, Int)] -> [Int] -> Bool
validUpdate rules update =
  let updatePairs =
        [ (update !! x, update !! y)
          | x <- [0 .. length update - 2],
            y <- [x .. length update - 1]
        ]
      brokenPairs = map (\(x, y) -> (y, x)) updatePairs
   in not (any (`elem` rules) brokenPairs)

middleElem :: [a] -> a
middleElem xs = xs !! (length xs `div` 2)

task1 :: String -> Int
task1 s =
  let (rules, updates) = parse s
      validUpdates = filter (validUpdate rules) updates
   in sum $ map middleElem validUpdates

fixUpdate :: [(Int, Int)] -> [Int] -> [Int]
fixUpdate rules update =
  let rules' = filter (\(a, b) -> a `elem` update && b `elem` update) rules
      edges = map (\a -> (a, a, [b | (f, b) <- rules', f == a])) update
      (graph, fromVertex, _) = G.graphFromEdges edges
      sortedValues = map ((\(x, _, _) -> x) . fromVertex) $ G.topSort graph
      compare' :: Int -> Int -> Ordering
      compare' a b = compare (elemIndex a sortedValues) (elemIndex b sortedValues)
   in sortBy compare' update

task2 :: String -> Int
task2 s =
  let (rules, updates) = parse s
      invalidUpdates = filter (not . validUpdate rules) updates
      fixedUpdates = map (fixUpdate rules) invalidUpdates
   in sum $ map middleElem fixedUpdates

main :: IO ()
main = do
  s <- readFile "d05.txt"
  print $ task1 s
  print $ task2 s