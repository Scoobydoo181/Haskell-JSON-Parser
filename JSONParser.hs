
{-# LANGUAGE DataKinds #-}
module JSONParser where

import Data.Char
import Data.List (dropWhileEnd, dropWhile)
import Control.Applicative

data JSONValue =
    JSONNull
    | JSONBool Bool
    | JSONInt Int
    | JSONString String
    | JSONArray [JSONValue]
    | JSONObject [(String, JSONValue)]
    deriving (Show, Eq)

newtype Parser t = Parser {
    runParser :: String -> Maybe (String, t)
}

instance Functor Parser where
    fmap f p = Parser $ \text -> do
        (rest, output) <- runParser p text
        return (rest, f output)

instance Applicative Parser where
    pure a = Parser $ \text -> Just (text, a)
    parserF <*> p = Parser $ \text -> do
        (rest, func) <- runParser parserF text
        runParser (fmap func p) rest

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser a) <|> (Parser b) = Parser $ \text -> a text <|> b text

charP :: Char -> Parser Char
charP ch = Parser f where
    f (x:xs)
        | x == ch    = Just (xs, x)
        | otherwise  = Nothing
    f _              = Nothing

textP :: String -> Parser String
textP = traverse charP

nullP :: Parser JSONValue
nullP = JSONNull <$ textP "null"

boolP :: Parser JSONValue
boolP = trueP <|> falseP where
    trueP = JSONBool True <$ textP "true"
    falseP = JSONBool False <$ textP "false"

spanP :: (Char -> Bool) -> Parser String
spanP fn = Parser $ \text ->
    let (match, rest) = span fn text in
    Just (rest, match) 

intP :: Parser JSONValue
intP = JSONInt . read <$> spanP isDigit

stringP :: Parser JSONValue
stringP = quoteP *>  (JSONString <$> spanNonQuoteP) <* quoteP where
    quoteP = charP '\''
    spanNonQuoteP = spanP (/= '\'')

wsP :: Parser String
wsP = spanP isSpace

arrayP :: Parser JSONValue
arrayP = fmap JSONArray $ charP '[' *> elementsP <* wsP <* charP ']' where
    elementsP = liftA2 (:) firstP (many elementP) <|> pure []
    firstP = wsP *> jsonP
    elementP = wsP *> charP ',' *> wsP *> jsonP

objectP :: Parser JSONValue
objectP = fmap JSONObject $ charP '{' *> wsP *> elementsP <* charP '}' where
    elementsP = liftA2 (:) elementP (many restP) <|> pure []
    elementP = liftA2 (,) (fmap getString $ stringP <* wsP) (charP ':' *> wsP *> jsonP <* wsP)
    restP =  charP ',' *> wsP *> elementP
    getString (JSONString x) = x

jsonP :: Parser JSONValue
jsonP = nullP <|> boolP <|> intP <|> stringP <|> arrayP <|> objectP