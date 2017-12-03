module Day02
  ( day02
  , day02parse
  , day02a
  , day02b
  ) where


day02a, day02b :: [[Int]] -> Int
day02a = common $ \ns -> maximum ns - minimum ns
day02b = common $ \ns -> head [x `div` y | x <- ns, y <- ns, x > y && x `rem` y == 0]

common :: ([Int] -> Int) -> [[Int]] -> Int
common f = sum . map f

day02parse :: String -> [[Int]]
day02parse = map (map read . words) . lines

day02 :: String -> IO (String, String)
day02 input =
  let rows = day02parse input
   in return (show . day02a $ rows, show . day02b $ rows)
