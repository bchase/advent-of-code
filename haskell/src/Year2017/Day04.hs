module Year2017.Day04 ( day04 ) where

import           Data.List.Unique (unique)
import qualified Data.Map.Strict  as Map
import           Data.Map.Strict  (Map)

import           Types


day04 :: Mode -> AB -> String -> IO [String]
day04 _ _ passphrase = do
  -- return [show . length . filter id . map hasNoDuplicateTokens . lines $ ps] -- PART 1
  -- return [show . length . filter id . map hasNoAnagramTokens . lines $ ps]   -- PART 2
  return $ map show [ hasNoDuplicateTokens passphrase
                    , hasNoAnagramTokens passphrase
                    ]

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
