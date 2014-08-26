Given a list of all valid English words and a list of words to check, a spell
checker's job is to find all invalid words in the second list.

We provide two spell checkers, one is a straight forward use of Data.Map, and
an exotic solution with a custom-built mutable trie. We will benchmark them.
We hope to see our trie outperforms Data.Map.

In the end, our trie is actually two times slower than Data.Map.

Features:

- Mutable data structure

Status: Completed