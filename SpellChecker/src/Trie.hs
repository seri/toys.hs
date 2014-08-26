module Trie(Trie, empty, insert, member) where

import Data.Char (ord, toUpper)
import Data.Maybe (isJust)
import Control.Monad.ST
import qualified Data.Vector.Mutable as V 

-- In a real world scenario, we would probably want to our mutable trie to base
-- on PrimMonad and PrimState so that it can work within both IO and ST. Also,
-- our trie only accepts String keys composed of capitalized English alphabets
-- ([A-Z]+). Values, though, can be of any type. Finally, there should have
-- been a function to retrieve the value given a key, but we omitted it because
-- of laziness (pun intended).
data Trie s a = Trie {
    trieValue :: Maybe a ,
    trieChildren :: V.STVector s (Maybe (Trie s a))
}

toIndex :: Char -> Int
toIndex c = (ord (toUpper c) - ord 'A') `mod` 26


empty :: ST s (Trie s a)
empty = emptyWith Nothing

newChildren :: ST s (V.STVector s (Maybe (Trie s a)))
newChildren = V.replicate 26 Nothing

emptyWith :: Maybe a -> ST s (Trie s a)
emptyWith x = newChildren >>= return . Trie x


insert :: Trie s a -> String -> a -> ST s (Trie s a)
insert = insert' . Just

insert' :: Maybe (Trie s a) -> String -> a -> ST s (Trie s a)
insert' Nothing cs z = do
    node <- empty
    insert node cs z
insert' (Just root@(Trie _ ys)) [c] z = do
    node <- emptyWith (Just z)
    V.write ys (toIndex c) (Just node)
    return root
insert' (Just root@(Trie _ ys)) (c:cs) z = do
    let i = toIndex c
    y <- V.read ys i
    insert' y cs z >>= V.write ys i . Just
    return root


member :: Trie s a -> String -> ST s Bool
member = member' . Just

member' :: Maybe (Trie s a) -> String -> ST s Bool
member' Nothing _ = return False
member' (Just (Trie x _)) [] = return (isJust x)
member' (Just (Trie _ ys)) (c:cs) = 
    V.read ys (toIndex c) >>= flip member' cs