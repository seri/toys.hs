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
data MoreExpr = NullExpr | MoreExpr AddSub Term MoreExpr
    deriving Show
data AddSub = Add | Sub
    deriving Show
data Term = Term Factor MoreTerm
    deriving Show
data MoreTerm = NullTerm | MoreTerm MulDiv Factor MoreTerm
    deriving Show
data MulDiv = Mul | Div
    deriving Show
data Factor = Number Int | Bracket Expr
    deriving Show