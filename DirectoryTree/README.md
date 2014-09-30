## Problem

Jon Snow is a Haskeller but there were no jobs for Haskellers in Winterfell.
Actually, there were no jobs for Haskellers anywhere in Westeros. He eventually
managed to get a job at King's Landing involving Java. His first assignment is
to maintain a legacy Java code base. 

Jon likes to `cd` around to navigate a new code base but this is a bit
unpleasant to do with `src/java/main/org/enterprise/foo/bar` so he hired you to
write a tool to display the whole source tree all at once. The tool should
support:

- The option to exclude certain directories because nobody wants to see the
mess inside `.git`.
- The option to automatically collapse long empty paths similar to Github.

## Lessons

- File system API
- Monoid
- Command line arguments parsing with optparse-applicative

## Dependencies

    $ cabal install optparse-applicative

## Status 

Done