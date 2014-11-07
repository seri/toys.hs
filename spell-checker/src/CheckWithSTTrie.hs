module CheckWithSTTrie (checkWithSTTrie) where

import SpellChecker (SpellChecker)
import qualified STTrie as T

import Control.Monad (foldM, filterM, liftM)
import Control.Monad.ST (ST(..), runST)

insert :: T.STTrie s Bool -> String -> ST s (T.STTrie s Bool)
insert trie key = T.insert trie key True

-- A spell checker using a mutable trie
checkWithSTTrie :: SpellChecker
checkWithSTTrie source target = runST $ do
    empty <- T.empty
    trie <- foldM insert empty source
    filterM (liftM not . T.member trie) target