Given a list of all valid English words and a list of words to check, a spell
checker's job is to find all invalid words in the second list. This is a
shameless an excuse to implement a mutable trie and to benchmark it against
similar data structres from the standard library.

Result of the benchmark: `Data.Trie` is the fastest, `Data.Set` comes pretty
close, `Data.Map` is about two times slower, and our `STTrie` is infinitely
slower than everything.

Features:

- Mutable data structure
- Force strict evaluation with deepseq

Requirements:

    $ cabal install vector
    $ cabal install containers
    $ cabal install bytestring-trie
    $ cabal install deepseq

Status: Working