module Main where

parse :: String -> [(Int, [Int])]
parse = map (parseLine . words) . lines
  where
    parseLine (x : l) = (read (take (length x - 1) x), map read l)

possiblyTrue :: (Int, [Int]) -> Bool
possiblyTrue (target, operands) =
  let go :: Int -> [Int] -> Bool
      go acc [] = acc == target
      go acc (x : xs) = (acc <= target) && (go (acc + x) xs || go (acc * x) xs)
   in go 0 operands

concat' :: Int -> Int -> Int
concat' a b = read $ show a ++ show b

possiblyTrue2 :: (Int, [Int]) -> Bool
possiblyTrue2 (target, operands) =
  let go :: Int -> [Int] -> Bool
      go acc [] = acc == target
      go acc (x : xs) =
        ((acc <= target) && (go (acc + x) xs || go (acc * x) xs || go (acc `concat'` x) xs))
   in go 0 operands

task1 :: String -> Int
task1 = sum . map fst . filter possiblyTrue . parse

task2 :: String -> Int
task2 = sum . map fst . filter possiblyTrue2 . parse

main :: IO ()
main = do
  s <- readFile "d07.txt"
  print $ task1 s
  print $ task2 s