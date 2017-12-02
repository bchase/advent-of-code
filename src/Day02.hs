module Day02
  ( day02a
  , day02b
  ) where


day02a, day02b :: String -> Int
day02a = day02 $ \ns -> maximum ns - minimum ns
day02b = day02 $ \ns -> head [x `div` y | x <- ns, y <- ns, x > y && x `rem` y == 0]

day02 :: ([Int] -> Int) -> String -> Int
day02 f = sum . map f . parseRows
  where parseRows = map (map read . words) . lines
