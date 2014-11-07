-- Using Aarsec, parse the arithmetic expressions defined in Expr 

module Parse (parse) where

import Expr
import Aarsec
import Data.Char
import Control.Applicative

expr :: Parser Expr
expr = liftA2 Expr term moreExpr

moreExpr :: Parser MoreExpr
moreExpr = (liftA3 MoreExpr addSub term moreExpr) <|> (pure NullExpr) 

addSub :: Parser AddSub
addSub = spaces *> (char '+' *> pure Add) <|> (char '-' *> pure Sub)

term :: Parser Term
term = liftA2 Term factor moreTerm

moreTerm :: Parser MoreTerm
moreTerm = (liftA3 MoreTerm mulDiv factor moreTerm) <|> (pure NullTerm)

mulDiv :: Parser MulDiv
mulDiv = spaces *> (char '*' *> pure Mul) <|> (char '/' *> pure Div)

factor :: Parser Factor
factor = bracketExpr <|> number

bracketExpr :: Parser Factor
bracketExpr = liftA Bracket ((spaces *> char '(') *> 
                     expr <* (spaces *> char ')'))

number :: Parser Factor
number = (liftA Number . collapse . liftA toInt)
         (spaces *> (many (satisfy isDigit)))

toInt :: String -> Maybe Int
toInt cs = if null cs then Nothing else Just (foldl1 f (map digitToInt cs)) where
    f x y = x * 10 + y

collapse :: Parser (Maybe a) -> Parser a
collapse (Parser p) = Parser q where
    q input = case p input of
        Nothing -> Nothing
        Just (Nothing, rest) -> Nothing
        Just (Just x, rest) -> Just (x, rest)
    
parse :: String -> Maybe Expr
parse = fmap fst . runParser expr

-- TODO: can we write collapse in applicative style?