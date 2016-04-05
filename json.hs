import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool
               | S String
               | F Float
               | N String
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
jsonFloat = F <$> float

nullLiteral :: Parser String
nullLiteral = (string "null") *> (pure "null")

jsonNullLiteral :: Parser JSONValue
jsonNullLiteral = lexeme $ N <$> nullLiteral

jsonValue :: Parser JSONValue
jsonValue = jsonBool <|> jsonStringLiteral <|> jsonFloat <|> jsonNullLiteral
