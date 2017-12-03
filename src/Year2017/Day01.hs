module Year2017.Day01 ( day01 ) where


day01a :: [Int] -> Int
day01a = common 1

day01b :: [Int] -> Int
day01b str = common (length str `quot` 2) str

common :: Int -> [Int] -> Int
common _ [] = 0
common times nums =
  sum . map fst . filter (\(n0,n1) -> n0 == n1) . zip (rotate times nums) $ nums
  where
    rotate :: Int -> [a] -> [a]
    rotate t xs = (drop t xs) ++ (take t xs)

day01parse :: String -> [Int]
day01parse = map (read . return)

day01 :: String -> [String]
day01 input =
  let nums = day01parse input
   in map show [ day01a nums, day01b nums ]
