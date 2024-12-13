module DayOne where

import Data.List (sort)

-- - AOC Day 1 Dummy Data
-- xs :: [Int]
-- xs = [3, 4, 2, 1, 3, 3]
--
-- ys :: [Int]
-- ys = [4, 3, 5, 3, 9, 3]

calcDistance :: (Num a, Ord a) => [a] -> [a] -> a
calcDistance xs ys = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

getData :: IO ([Int], [Int])
getData = do
  f <- readFile "./data/day_one.txt"
  return $ unzip $ map ((\x -> (head x, x !! 1)) . map (\s -> read s :: Int) . words) (lines f)

similarity :: [Int] -> [Int] -> Int
similarity xs ys = sum $ map (\x -> x * length (filter (x ==) ys)) xs

solveDayOne :: IO ()
solveDayOne = do
  (xs, ys) <- getData
  print $ calcDistance xs ys
  print $ similarity xs ys
