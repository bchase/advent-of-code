module Main where

import           System.Environment (getArgs)

import           Lib


main :: IO ()
main = getArgs >>= putStrLn . show . day01a . head
