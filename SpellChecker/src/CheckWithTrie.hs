module CheckWithTrie (checkWithTrie) where

import SpellChecker (SpellChecker)
import qualified Trie

import Control.Monad (foldM, filterM)
import Control.Monad.ST (ST(..), runST)

insert :: Trie.Trie s Bool -> String -> ST s (Trie.Trie s Bool)
insert trie key = Trie.insert trie key True

checkWithTrie :: SpellChecker
checkWithTrie source target = runST $ do
    empty <- Trie.empty
    trie <- foldM insert empty source
    invalids <- filterM (Trie.member trie) target 
    return $ length invalids