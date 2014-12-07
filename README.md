# The Eloquence Language
## List Basics
### `()`
is the empty list.
### `quote`
is a function used to forego evaluation.
For example, `(quote (1 2 3 4))`
is like saying a list holding `1`, `2`, `3` and `4`.
### `cons`
prepends an element to a list.
For example, `(cons 42 ())`
is `(42)`.
Remember, lists are values too!
So, `(cons () ())`
is `(())`.
### `tail`
returns a list containing all but the first element
so, `(tail (quote (1 2 3 4)))`
is `(2 3 4)`.
if an empty list is given to `tail`
an empty list is returned.
### `first`
returns the first element of a list.
For example, `(first (quote (1 2 3 4 5)))`
is `1`.
if an empty list is given to `first`
an empty list is returned.
## Checking Values
### `atom?`
is truthy, when given anything that is not a list.
For example, `(atom? 42)`
is truthy.
It returns the falsey value (aka the empty list), when given a list.
For example, `(atom? ())`
is falsey.
### `eq?`
is truthy, when given two of the same thing.
For example, `(eq? 1 1)`
is truthy.
Is falsey, when given two different things.
For example, `(eq? 42 ())`
is falsey.
`eq?` Performs a by-value compare
so, `(eq? (quote (1 2 3)) (quote (1 2 3)))`
is truthy.
### `cond`
takes pairs of comparison-result pairs, returning the first truthy one.
For example, `(cond () 37 1 42)`
evaluates to `42` (since the empty list is falsey)
If no truthy values are given
returns the empty list.
### AND MORE
docs are a work in progress :). For more docs ATM, see `TestList.hs`.

## What's in `core`?
Nothing really, at the moment. There are just examples and the results of successful sandboxing.

## To Build
Get Haskell platform
```
sudo apt-get install haskell-platform
```

Build it!
```
ghc Eloquence.hs -o elo
```

This generates the `elo` executable (the REPL) that can be run with
```
./elo
```

## Running Tests
Ensure HSpec is installed
```
cabal update && cabal install hspec
```

Run tests!
```
runhaskell Test<file>.hs
```
