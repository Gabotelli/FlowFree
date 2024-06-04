@echo off
cabal v2-build
.\dist-newstyle\build\x86_64-linux\ghc-8.8.4\haskell-0.1.0.0\x\haskell\build\haskell\haskell > src-exe\output.svg -w 400