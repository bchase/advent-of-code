{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Day02
  ( day02a
  ) where

import           Text.Regex.PCRE.Heavy


day02a :: String -> Int
day02a str =
  let strToInts = map (read :: String -> Int) . map fst . scan [re|\d+|]
      intRows = map strToInts . lines $ str
   in sum . map (\ns -> maximum ns - minimum ns) $ intRows
