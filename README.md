## What are toys

A toy is a runnable program that has these properties:

- *Small*: There should be less than 10 source files.
- *Motivating*: It should do something easily understandable and imaginatively
useful.
- *Educational*: It should make use of something interesting from the Haskell
ecosystem and it must be written in idiomatic Haskell.

## How to run toys

If a toy has a Makefile, do:

    $ make

Otherwise you can shamelessly go:

    $ runhaskell <ToyName>.hs [arguments]

## List of completed toys

Here is a list of completed toys in (non-strictly) increasing order of
difficulty:

- Internet time
- Directory tree
- Reddit client
- Spell checker
- Calculator