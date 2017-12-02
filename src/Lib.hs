module Lib
    ( day01a
    ) where

import           Text.Read  (readMaybe)
import           Data.Maybe (catMaybes)


day01a :: String -> Int
day01a "" = 0
day01a str =
  let nums@(n:ns) = catMaybes . map (\ch -> readMaybe [ch] :: Maybe Int) $ str
   in sum . map fst . filter (\(n0,n1) -> n0 == n1) . zip (ns ++ [n]) $ nums
