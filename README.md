# The Eloquence Language
## List Basics
### `()`
is the empty list
### `quote`
is a function used to forego evaluation.
For example, `(quote (1 2 3 4))`
is like saying a list holding `1`, `2`, `3` and `4`
### `cons`
prepends an element to a list
For example, `(cons 42 ())`
is `(42)`
Remember, lists are values too!
So, `(cons () ())`
is `(())`
### `tail`
returns a list containing all but the first element
so, `(tail (quote (1 2 3 4)))`
is `(2 3 4)`
if an empty list is given to `tail`
an empty list is returned
### `first`
returns the first element of a list
For example, `(first (quote (1 2 3 4 5)))`
is `1`
if an empty list is given to `first`
an empty list is returned
### AND MORE... docs just aren't ready :)
docs are a work in progress
# PENDING: No reason given

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
