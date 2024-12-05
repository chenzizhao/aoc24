module Main where

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diff :: (Num a) => [a] -> [a]
diff [] = []
diff ls = zipWith (-) (tail ls) ls

safe :: [Int] -> Bool
safe l =
  let d = diff l
   in all (\x -> 1 <= abs x && abs x <= 3) d && (all (> 0) d || all (< 0) d)

safe' :: [Int] -> Bool
safe' l = any (safe . (\i -> take i l ++ drop (i + 1) l)) [0 .. length l - 1]

task1 :: String -> Int
task1 = length . filter safe . parse

task2 :: String -> Int
task2 = length . filter safe' . parse

main :: IO ()
main = do
  input <- readFile "d02.txt"
  print $ task1 input
  print $ task2 input