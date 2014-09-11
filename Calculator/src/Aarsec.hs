-- A minimal applicative parsing library

module Aarsec ( 
    Parser(..) , 
    token , satisfy , char , string , spaces
) where

import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad (join)
import Control.Arrow ((***))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (f *** id) . p)

instance Applicative Parser where
    pure x = Parser (Just . ((,) x))
    Parser pf <*> q = Parser (join . fmap g . pf) where
        g (f, input) = runParser (fmap f q) input

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p) <|> (Parser q) = Parser (foldl1 (<|>) . (<*>) [p, q] . (: []))

token :: Parser Char
token = Parser p where
    p [] = Nothing
    p (c:cs) = Just (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser (join . fmap f . runParser token) where
    f (c, cs) = if pred c then Just (c, cs)
                          else Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = liftA2 (:) (char c) (string cs)

spaces :: Parser String
spaces = many (satisfy isSpace)