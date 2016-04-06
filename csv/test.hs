module Main where

import Csv
import Text.ParserCombinators.Parsec (parseFromFile)

main :: IO ()
main = do
  testValue <- parseFromFile table "test.csv"
  print testValue
