#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc

import Data.Set (Set)
import qualified Data.Set as Set
-- import Debug.Trace (trace)

main :: IO ()
main = do
  txt <- readFile "aoc201801a.txt"
  let input = parse txt
  print $ finalFreq input

  -- print $ firstDupFreq [0] [ 1, (-2), 3 ]             -- 4
  -- print $ firstDupFreq [0] [ 1, (-1)             ]    -- 0
  -- print $ firstDupFreq [0] [ 3, 3, 4, (-2), (-4) ]    -- 10
  -- print $ firstDupFreq [0] [ (-6), 3, 8, 5, (-6) ]    -- 5
  -- print $ firstDupFreq [0] [ 7, 7, (-2), (-7), (-4) ] -- 14
  print $ firstDupFreq [0] input

  where
    parse :: String -> [Int]
    parse = map (read . dropWhile (== '+')) . lines



---- A ----

finalFreq :: [Int] -> Int
finalFreq = foldr (+) 0



---- B ----

firstDupFreq :: [Int] -> [Int] -> Int
firstDupFreq fs ns =
  case firstDup . reverse $ freqs of
    Nothing -> firstDupFreq freqs ns
    Just f  -> f
  where
    freqs = foldl (\acc n -> (head acc + n) : acc) fs ns

firstDup :: ( Ord a ) => [a] -> Maybe a
firstDup = firstDup' Set.empty
  where
    firstDup' :: ( Ord a ) => Set a -> [a] -> Maybe a
    firstDup' _  [] = Nothing
    firstDup' ys (x:xs)
      | Set.member x ys = Just x
      | otherwise       = firstDup' (Set.insert x ys) xs
