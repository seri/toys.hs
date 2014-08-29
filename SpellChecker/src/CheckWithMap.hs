module CheckWithMap (checkWithMap) where

import SpellChecker (SpellChecker)

import qualified Data.Map.Strict as M

-- A spell checker using plain old Data.Map
checkWithMap :: SpellChecker
checkWithMap source = filter (not . flip M.member dict) where
    dict = foldl insert (M.empty :: M.Map String Bool) source 
    insert dict word = M.insert word True dict