module Year2017.Day02 ( day02 ) where

import           Safe (headMay)


day02a, day02b :: [[Int]] -> Int
day02a = common $ \ns -> maximum ns - minimum ns
day02b = common $ \ns ->
  maybe 0 id $ headMay [x `div` y | x <- ns, y <- ns, x > y && x `rem` y == 0]

common :: ([Int] -> Int) -> [[Int]] -> Int
common f = sum . map f

day02parse :: String -> [[Int]]
day02parse = map (map read . words) . lines

day02 :: String -> [String]
day02 input =
  let rows = day02parse input
   in map show [ day02a rows, day02b rows ]
