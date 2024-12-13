module DayTwo where

ns :: [[Int]]
ns =
  [ [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
  ]

getData :: IO [[Int]]
getData = do
  f <- readFile "./data/day_two.txt"
  return $ map (map (\y -> read y :: Int) . words) (lines f)

getDiffs :: (Num a) => [a] -> [a]
getDiffs (x : y : xs) = (x - y) : getDiffs (y : xs)
getDiffs _ = []

isInRange :: (Ord a, Num a) => [a] -> [Bool]
isInRange (x : xs) = (x > 0 && x <= 3 || x < 0 && x >= -3) : isInRange xs
isInRange _ = []

isDecreasing :: (Ord a, Num a) => [a] -> [Bool]
isDecreasing = map (>= 0)

isIncreasing :: (Ord a, Num a) => [a] -> [Bool]
isIncreasing = map (< 0)

isOneWay :: (Ord a, Num a) => [a] -> Bool
isOneWay xs = and (isIncreasing xs) && not (and (isDecreasing xs)) || not (and (isIncreasing xs)) && and (isDecreasing xs)

allIsInRange :: (Ord a, Num a) => [a] -> Bool
allIsInRange xs = and (isInRange xs)

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe xs = isOneWay xs && allIsInRange xs

remOneLevelDiff :: (Ord a, Num a) => Bool -> [a] -> [a]
remOneLevelDiff b (x : y : xs)
  | abs (x - y) <= 3 && x - y /= 0 = x : y : remOneLevelDiff b xs
  | not (abs (x - y) <= 3 && x - y /= 0) && b = x : y : remOneLevelDiff True (x : xs)
  | not (abs (x - y) <= 3 && x - y /= 0 && not b) = x : remOneLevelDiff True xs
  | otherwise = []
remOneLevelDiff _ [x] = [x]
remOneLevelDiff _ [] = []

-- This is bad and only half done bc sleepy
solveDayTwo :: IO ()
solveDayTwo = do
  -- print $ length $ filter id $ map (isSafe . getDiffs . remOneLevelDiff False) ns

  print (ns !! 3)
  print (remOneLevelDiff False (ns !! 3))
