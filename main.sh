#!/bin/bash
cabal v2-build
cat src-exe/Juego.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.8.4/haskell-0.1.0.0/x/haskell/build/haskell/haskell -o ./src-exe/output.svg -w 400