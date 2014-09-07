-- A minimal applicative parsing library

module Aarsec where

import Data.Functor
import Control.Applicative
import Control.Monad (join)
import Control.Arrow ((***))

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (f *** id) . p)

instance Applicative Parser where
    pure x = Parser (Just . ((,) x))
    Parser pf <*> q = Parser (join . fmap g . pf) where
        g (f, input) = parse (fmap f q) input

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p) <|> (Parser q) = Parser (foldl1 (<|>) . (<*>) [p, q] . (: []))

