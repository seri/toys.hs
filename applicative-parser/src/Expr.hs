-- Data type definition for arithmetic expresisons

module Expr ( 
    Expr (..) , 
    MoreExpr (..) ,
    AddSub (..) ,
    Term (..) ,
    MoreTerm (..) ,
    MulDiv (..) ,
    Factor (..)
) where

-- This representation makes it harder to evaluate but easier to parse

data Expr = Expr Term MoreExpr 
    deriving Show
data MoreExpr = MoreExpr AddSub Term MoreExpr | NullExpr
    deriving Show
data AddSub = Add | Sub
    deriving Show
data Term = Term Factor MoreTerm
    deriving Show
data MoreTerm = MoreTerm MulDiv Factor MoreTerm | NullTerm
    deriving Show
data MulDiv = Mul | Div
    deriving Show
data Factor = Bracket Expr | Number Int
    deriving Show