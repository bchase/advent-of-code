module Main where

-- import           Data.Maybe         (maybe)
import qualified Data.Map           as Map
import           Data.Map           (Map, fromList)
import           System.Environment (getArgs)

import           Year2017


type Year = Int
type Day  = Int

main :: IO ()
main = readArgs >>= maybe printUsage run
  where
    printUsage = putStrLn "$ aoc YEAR DAY INPUT"

    run (year, day, input) = do
      case Map.lookup (year, day) days of
        Nothing -> putStrLn $ "Not a valid day: " ++ (show day)
        Just f  -> f input >>= \(a,b) -> print ("A: " ++ a, "B: " ++ b)

    readArgs :: IO (Maybe (Year, Day, String))
    readArgs = getArgs >>= \args ->
      case args of
        [y,d,i] -> return $ Just (read y, read d, i)
        _       -> return Nothing

days :: Map (Year, Day) (String -> IO (String, String))
days =
  fromList
    [ ((2017, 1), day01)
    , ((2017, 2), day02)
    ]
