module Main where

import qualified Data.Map           as Map
import           Data.Map           (Map, fromList)
import           System.Environment (getArgs)

import           Lib


main :: IO ()
main = do
  putStrLn "$ aoc (day :: Int) (input :: String)"
  args <- getArgs
  let day = read . head $ args :: Int
      input = head . tail $ args
      mFunc = Map.lookup day days
  case mFunc of
    Nothing -> putStrLn $ "Not a valid day: " ++ (show day)
    Just f  -> f input >>= \(a,b) -> print ("A: " ++ a, "B: " ++ b)


days :: Map Int (String -> IO (String, String))
days =
  fromList
    [ (1, day01)
    , (2, day02)
    ]
