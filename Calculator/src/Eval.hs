-- Evaluate arithmetic expressions defined in Expr

module Eval (eval) where

import Expr

eval :: Expr -> Double
eval = evalExpr

evalExpr :: Expr -> Double
evalExpr (Expr term moreExpr) = evalExpr' (evalTerm term) moreExpr

evalExpr' :: Double -> MoreExpr -> Double
evalExpr' x NullExpr = x
evalExpr' x (MoreExpr Add term moreExpr) = evalExpr' (x + evalTerm term) moreExpr
evalExpr' x (MoreExpr Sub term moreExpr) = evalExpr' (x - evalTerm term) moreExpr

evalTerm :: Term -> Double
evalTerm (Term factor moreTerm) = evalTerm' (evalFactor factor) moreTerm

evalTerm' :: Double -> MoreTerm -> Double
evalTerm' x NullTerm = x
evalTerm' x (MoreTerm Mul factor moreTerm) = evalTerm' (x * evalFactor factor) moreTerm
evalTerm' x (MoreTerm Div factor moreTerm) = evalTerm' (x / evalFactor factor) moreTerm

evalFactor :: Factor -> Double
evalFactor (Number n) = fromIntegral n
evalFactor (Bracket expr) = evalExpr expr