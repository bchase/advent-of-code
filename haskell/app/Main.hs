module Main where

import qualified Data.Text          as T
import           Data.Maybe         (maybe)
import qualified Data.Map           as Map
import           Data.Map           (Map, fromList)
import           System.Environment (getArgs)

import           Types
import           Year2017


type Year = Int
type Day  = Int

main :: IO ()
main = readArgs >>= maybe (putStrLn "ERROR") run
  where
    readArgs :: IO (Maybe (Mode, Year, Day, AB, String))
    readArgs = getArgs >>= \args -> do
      case args of
        [m,y,d,ab,f] -> do
          i <- readFile f >>= return . T.unpack . T.strip . T.pack
          return $ Just (read m, read y, read d, read ab, i)
        _ -> return Nothing

    run :: (Mode, Year, Day, AB, String) -> IO ()
    run (m,y,d,ab,i) = maybe (invalid y d) (\f -> f m ab i >>= mapM_ putStrLn) $ Map.lookup (y,d) days

    invalid y d = putStrLn $ "Not a valid year/day: " ++ (show y) ++ "/" ++ (show d)


days :: Map (Year, Day) (Mode -> AB -> String -> IO [String])
days =
  fromList
    -- [ ((2017, 1), return . day01)
    -- , ((2017, 2), return . day02)
    -- , ((2017, 3), day03)
    -- , ((2017, 4), day04)
    -- ]
    [ ((2017, 3), day03)
    , ((2017, 4), day04)
    , ((2017, 5), day05)
    , ((2017, 6), day06)
    ]
