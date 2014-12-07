Eloquence
==============

A toy lisp implementation in Haskell.

## Language Features
See `TestLisp.hs` for specification of language.

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
runhaskell Test*
```
