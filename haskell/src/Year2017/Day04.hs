module Year2017.Day04 ( day04 ) where

import           Data.List.Unique (unique)
import qualified Data.Map.Strict  as Map
import           Data.Map.Strict  (Map)

import           Types


day04 :: Mode -> AB -> String -> IO [String]
day04 mode ab input = do
  case mode of
    Test -> return [ show . check ab $ input ]
    Run  -> return [ show . length . filter id . map (check ab) . lines $ input ]
  where
    check ab =
      case ab of
        A -> hasNoDuplicateTokens
        B -> hasNoAnagramTokens

hasNoDuplicateTokens :: String -> Bool
hasNoDuplicateTokens = isUniqueList . words

hasNoAnagramTokens :: String -> Bool
hasNoAnagramTokens = isUniqueList . map getCharCounts . words
  where
    getCharCounts :: String -> [(String, Int)]
    getCharCounts = Map.toList . foldr incCharCount (Map.fromList [])

    incCharCount :: Char -> Map String Int -> Map String Int
    incCharCount ch m = let n = Map.findWithDefault 0 [ch] m in Map.insert [ch] (n+1) m

isUniqueList :: (Ord a) => [a] -> Bool
isUniqueList ps = length ps == length (unique ps)
