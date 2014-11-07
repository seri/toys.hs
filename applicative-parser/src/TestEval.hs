-- Used internally during development just to get used to the Expr data type

module TestEval (testEval) where

import Expr
import Eval

oneTerm = Term (Number 1) NullTerm
twoTerm = Term (Number 2) NullTerm
threeTerm = Term (Number 3) NullTerm
oneAddTwoExpr = Expr oneTerm (MoreExpr Add twoTerm NullExpr)
twoSubOneExpr = Expr twoTerm (MoreExpr Sub oneTerm NullExpr)

testCases :: [(Expr, Double)]
testCases = 
    [ (Expr oneTerm (MoreExpr Add twoTerm (MoreExpr Add threeTerm NullExpr)), 6) 
    , (Expr threeTerm (MoreExpr Sub twoTerm (MoreExpr Sub oneTerm NullExpr)), 0)
    , (Expr threeTerm (MoreExpr Sub (Term (Bracket twoSubOneExpr) NullTerm) NullExpr), 2)
    , (Expr oneTerm (MoreExpr Add (Term (Number 2) (MoreTerm Mul (Number 3) NullTerm)) NullExpr), 7)
    , (Expr (Term (Bracket oneAddTwoExpr) (MoreTerm Mul (Number 3) NullTerm)) NullExpr, 9)
    , (Expr (Term (Number 1) (MoreTerm Div (Number 2) NullTerm)) NullExpr, 0.5) ]

eachCase :: (Expr, Double)  -> IO ()
eachCase (expr, result) = (putStrLn . concat) 
    [ show expr, " = ", show (eval expr), " (expect ", show result, ")" ]

-- To actually run the tests, import TestEval in Main and call testEval in main

testEval :: IO ()
testEval = mapM_ eachCase testCases