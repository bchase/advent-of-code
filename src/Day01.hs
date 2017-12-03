module Day01
  ( day01a
  , day01b
  ) where


day01a :: String -> Int
day01a = day01 1

day01b :: String -> Int
day01b str = day01 (length str `quot` 2) str

day01 :: Int -> String -> Int
day01 _ "" = 0
day01 times str =
  let nums = map (read . return) str
   in sum . map fst . filter (\(n0,n1) -> n0 == n1) . zip (rotate times nums) $ nums
  where
    rotate :: Int -> [a] -> [a]
    rotate t xs = (drop t xs) ++ (take t xs)
