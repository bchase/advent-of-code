module Main where

import           System.Environment (getArgs)

import           Lib


main :: IO ()
main = getArgs >>= print . day02a . head
