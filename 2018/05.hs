#!/usr/bin/env stack
-- stack runghc --resolver lts-9.14 --install-ghc

import Data.Char (isUpper, isLower, toLower, toUpper)
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sortBy)

type Polymer = String



main :: IO ()
main = do
  -- testA
  -- testB

  polymer <- head . lines <$> readFile "input/05.txt"

  -- print $ fullyReactedLength polymer

  mapM_ print $ fullyReactedLengthsOfModified polymer

  return ()


---- B ----

fullyReactedLengthsOfModified :: Polymer -> [(Char, Int)]
fullyReactedLengthsOfModified =
  fullyReactedLengthsOfModified' ['a'..'z']

fullyReactedLengthsOfModified' :: [Char] -> Polymer -> [(Char, Int)]
fullyReactedLengthsOfModified' cs p =
  sortBy (compare `on` snd) . map (second fullyReactedLength) $ ps
  where ps = map (\ch -> (ch, remove ch p)) cs

remove :: Char -> Polymer -> Polymer
remove c = filter (\ch -> ch /= toLower c && ch /= toUpper c)


---- A ----

fullyReactedLength :: Polymer -> Int
fullyReactedLength = length . react

react :: Polymer -> Polymer
react p
  | p == p'    =  p
  | otherwise  =  react p'
  where p' = react' p

react' :: Polymer -> Polymer
react' (x:y:xs)
  | x `cancels` y = react' xs
  | otherwise     = x : react' (y:xs)
react' p = p

cancels :: Char -> Char -> Bool
cancels c1 c2 =
  same && ((upper1 && lower2) || (lower1 && upper2))
  where
    same   = toLower c1 == toLower c2
    upper1 = isUpper c1
    upper2 = isUpper c2
    lower1 = isLower c1
    lower2 = isLower c2



---- TEST ----

testA :: IO ()
testA = do
  let testPolymer' = react testPolymer

  print $ testPolymer' == testPolymerFinal

  putStrLn "v got"
  print testPolymer'
  print testPolymerFinal
  putStrLn "^ expected"

testB :: IO ()
testB = do
  mapM_ print (fullyReactedLengthsOfModified' ['a'..'d'] testPolymer)




testPolymer :: String
testPolymer = "dabAcCaCBAcCcaDA"
testPolymerFinal :: String
testPolymerFinal = "dabCBAcaDA"
