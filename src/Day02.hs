{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day02
  ( day02a
  , day02b
  ) where

import           Text.Regex.PCRE.Heavy


day02a :: String -> Int
day02a = sum . map (\ns -> maximum ns - minimum ns) . parseRows

day02b :: String -> Int
day02b = sum . map quotientOfEvenlyDivisiblePairs . parseRows
  where
    quotientOfEvenlyDivisiblePairs ns =
      head $ [x `quot` y | x <- ns, y <- ns, x > y && x `rem` y == 0]

parseRows :: String -> [[Int]]
parseRows = map ints . lines
  where ints = map read . map fst . scan [re|\d+|]
