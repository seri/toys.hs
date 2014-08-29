module STTrie(STTrie, empty, insert, STTrie.lookup, member) where

import Data.Char (ord, toUpper)
import Data.Maybe (isJust)
import Control.Monad (liftM)
import Control.Monad.ST (ST(..))
import qualified Data.Vector.Mutable as V 

-- In a real world scenario, we would probably want to our mutable trie to base
-- on PrimMonad and PrimState so that it can work within both IO and ST. Also,
-- our trie only accepts String keys composed of capitalized English alphabets
-- ([A-Z]+). Values, though, can be of any type.
data STTrie s a = STTrie {
    trieValue :: Maybe a ,
    trieChildren :: V.STVector s (Maybe (STTrie s a))
}

toIndex :: Char -> Int
toIndex c = (ord (toUpper c) - ord 'A') `mod` 26


empty :: ST s (STTrie s a)
empty = emptyWith Nothing

newChildren :: ST s (V.STVector s (Maybe (STTrie s a)))
newChildren = V.replicate 26 Nothing

emptyWith :: Maybe a -> ST s (STTrie s a)
emptyWith x = newChildren >>= return . STTrie x


insert :: STTrie s a -> String -> a -> ST s (STTrie s a)
insert = insert' . Just

insert' :: Maybe (STTrie s a) -> String -> a -> ST s (STTrie s a)
insert' Nothing cs z = do
    node <- empty
    insert node cs z
insert' (Just root@(STTrie _ ys)) [c] z = do
    node <- emptyWith (Just z)
    V.write ys (toIndex c) (Just node)
    return root
insert' (Just root@(STTrie _ ys)) (c:cs) z = do
    let i = toIndex c
    y <- V.read ys i
    insert' y cs z >>= V.write ys i . Just
    return root


lookup :: STTrie s a -> String -> ST s (Maybe a)
lookup = lookup' . Just

lookup' :: Maybe (STTrie s a) -> String -> ST s (Maybe a)
lookup' Nothing _ = return Nothing
lookup' (Just (STTrie x _)) [] = return x
lookup' (Just (STTrie _ ys)) (c:cs) = V.read ys (toIndex c) >>= flip lookup' cs


member :: STTrie s a -> String -> ST s Bool
member trie = liftM isJust . STTrie.lookup trie