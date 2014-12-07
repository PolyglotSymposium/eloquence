#!/bin/bash

runhaskell FunctionalityDocTests.hs | grep -v "PENDING" | grep -v "failures" | grep -v "Finished" | sed '/^$/d' | sed 's/^\s\+//' > README.md
cat readme_body >> README.md
