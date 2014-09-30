## Problem

Jon Snow is in charge of finance at the Wall so it is no wonder he has to
calculate a lot. Most GUI calculators are too slow for him so he he hired you
to write a fast and responsive CLI calculator. He even wrote some detailed use
case scenarios:

    $ make
    Welcome to Awesome Calculator
    Type exit to quit
    1 + 1
    Just 2
    1 + 2 * 3
    Just 7
    3 - 2 - 1
    Just 0
    3 - (2 - 1)
    Just 2
    exit

Jon also forbid you to use Monad because he heard on the news that Monads are
impure and thus attract the white walkers. The only Monads you are permitted to
use are `Maybe` and `IO`.

## Lessons

- Implement a primitive parser library (a Parsec wannabe) from scrach
- Construct an LL(1) grammar AST for arithmetic expressions
- Write a recursive descent parser
- Familiarity with Applicative style
- A taste of Arrow
- Program's main loop

## Status

Done

## References

- http://stackoverflow.com/questions/20660782/writing-a-parser-from-scratch-in-haskell
- http://knuth.luther.edu/~leekent/tutorials/ll1.html