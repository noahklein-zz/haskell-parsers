import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool
               deriving (Show)

ws :: Parser String
ws = many (oneOf " \t\n")

lexeme p = p <* ws

boolTrue :: Parser Bool
boolTrue = (string "true") *> (pure True)

boolFalse :: Parser Bool
boolFalse = (string "false") *> (pure False)

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = lexeme $ B <$> bool

jsonValue :: Parser JSONValue
jsonValue = jsonBool
