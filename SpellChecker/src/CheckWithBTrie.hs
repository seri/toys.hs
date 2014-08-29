module CheckWithBTrie (checkWithBTrie) where

import SpellChecker (SpellChecker)
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as B

-- A spell checker using the hackage bytestring-trie
checkWithBTrie :: SpellChecker
checkWithBTrie source = filter isInvalid where
    isInvalid = not . flip T.member trie . B.pack
    trie = foldl insert (T.empty :: T.Trie Bool) source 
    insert trie word = T.insert (B.pack word) True trie