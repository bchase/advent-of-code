module Main where

import           Data.Maybe         (maybe)
import qualified Data.Map           as Map
import           Data.Map           (Map, fromList)
import           System.Environment (getArgs)

import           Year2017


type Year = Int
type Day  = Int

main :: IO ()
main = readArgs >>= maybe printUsage run
  where
    readArgs :: IO (Maybe (Year, Day, String))
    readArgs = getArgs >>= \args ->
      case args of
        [y,d,i] -> return $ Just (read y, read d, i)
        _       -> return Nothing

    run :: (Year, Day, String) -> IO ()
    run (y,d,i) = maybe (invalid y d) (\f -> f i >>= mapM_ putStrLn) $ Map.lookup (y,d) days

    printUsage = putStrLn "$ aoc YEAR DAY INPUT"
    invalid y d = putStrLn $ "Not a valid year/day: " ++ (show y) ++ "/" ++ (show d)


days :: Map (Year, Day) (String -> IO [String])
days =
  fromList
    [ ((2017, 1), return . day01)
    , ((2017, 2), return . day02)
    ]
