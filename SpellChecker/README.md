Given a list of all valid English words and a list of words to check, a spell
checker's job is to find all invalid words in the second list.

This spell checker is just an excuse to implement a mutable trie. We will
benchmark it against Data.Map and Data.Trie. 

In the end, Data.Trie is the fastest (even though it has to convert between
String and ByteString), Data.Map comes second, and our mutable trie turns out
much slower than both of them. This is interesting because both Data.Trie and
Data.Map are purely functional data structures.

Features:

- Mutable data structure
- Force strict evaluation with deepseq

Requirements:

    $ cabal install vector
    $ cabal install containers
    $ cabal install bytestring-trie
    $ cabal install deepseq

Status: Working