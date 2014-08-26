module StringUtil (pluralize) where

pluralize :: Int -> String -> String
pluralize n s
    | n > 1 = result ++ "s"
    | otherwise = result
    where result = show n ++ " " ++ s