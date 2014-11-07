import Expr
import Eval
import Parse
import System.IO

mainLoop :: IO ()
mainLoop = do
    input <- getLine
    if input == "exit" 
        then return ()
        else do (print . fmap eval . parse) input >> mainLoop

main :: IO ()
main = do
    putStrLn "Welcome to Awesome Calculator"
    putStrLn "Type exit to quit"
    mainLoop