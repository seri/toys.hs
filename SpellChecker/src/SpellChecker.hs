module SpellChecker (SpellChecker) where

type SpellChecker 
    =  [String] -- list of all valid words called the source
    -> [String] -- list of words to check called the target
    -> Int      -- number of invalid words