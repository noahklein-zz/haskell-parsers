module Json (jsonValue) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool
               | S String
               | F Float
               | N String
               | A [JSONValue]
               | O [(String, JSONValue)]
               deriving (Show)

a <:> b = (:) <$> a <*> b
a <++> b = (++) <$> a <*> b

ws :: Parser String
ws = many (oneOf " \t\n")

lexeme :: Parser a -> Parser a
lexeme p = p <* ws

boolTrue :: Parser Bool
boolTrue = (string "true") *> (pure True)

boolFalse :: Parser Bool
boolFalse = (string "false") *> (pure False)

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = lexeme $ B <$> bool

stringLiteral :: Parser String
stringLiteral = char '"' *> (many (noneOf ['"'])) <* char '"'

jsonStringLiteral :: Parser JSONValue
jsonStringLiteral = lexeme $ S <$> stringLiteral

number :: Parser String
number = many1 digit

int :: Parser String
int = positive <|> negative <|> number
    where
      positive = char '+' *> number
      negative = char '-' <:> number

float :: Parser Float
float = fmap rd (int <++> decimal)
    where
        rd      = read :: String -> Float
        decimal = option "" $ char '.' <:> number

jsonFloat :: Parser JSONValue
jsonFloat = lexeme $ F <$> float

nullLiteral :: Parser String
nullLiteral = (string "null") *> (pure "null")

jsonNullLiteral :: Parser JSONValue
jsonNullLiteral = lexeme $ N <$> nullLiteral

comma :: Parser Char
comma = char ','

array :: Parser [JSONValue]
array = do
  lexeme $ char '['
  list <- (lexeme jsonValue) `sepBy` (lexeme comma)
  lexeme $ char ']'
  return list

jsonArray :: Parser JSONValue
jsonArray = A <$> array

objectEntry :: Parser (String, JSONValue)
objectEntry = do
    key <- stringLiteral
    lexeme $ char ':'
    value <- jsonValue
    return (key, value)

object :: Parser [(String, JSONValue)]
object = do
  lexeme $ char '{'
  list <- objectEntry `sepBy` (lexeme comma)
  lexeme $ char '}'
  return list

jsonObject :: Parser JSONValue
jsonObject = O <$> object

jsonValue :: Parser JSONValue
jsonValue = jsonBool <|> jsonStringLiteral <|> jsonFloat <|> jsonNullLiteral <|> jsonArray <|> jsonObject
