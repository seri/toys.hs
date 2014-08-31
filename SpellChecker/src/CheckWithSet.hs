module CheckWithSet (checkWithSet) where

import SpellChecker (SpellChecker)

import qualified Data.Set as S

-- A spell checker using plain old Data.Set
checkWithSet :: SpellChecker
checkWithSet source = filter (not . flip S.member set) where
    set = foldl (flip S.insert) (S.empty :: S.Set String) source