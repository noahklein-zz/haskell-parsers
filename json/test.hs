module Main where

import Json
import Text.ParserCombinators.Parsec (parseFromFile)

main :: IO ()
main = do
  testValue <- parseFromFile jsonValue "test.json"
  print testValue
