module Main where

import           System.Environment (getArgs)

import           Lib


main :: IO ()
main = getArgs >>= print . day01b . head
