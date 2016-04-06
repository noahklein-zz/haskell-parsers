module Csv (table) where

import Text.ParserCombinators.Parsec

type Cell = String
comma = char ','
nl = char '\n'

cell :: Parser Cell
cell = many (noneOf [',', '\n'])

row :: Parser [Cell]
row = cell `sepBy` comma

table :: Parser [[Cell]]
table = row `sepBy` nl
