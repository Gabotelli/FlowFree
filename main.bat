@echo off
echo cabal build
cabal build
IF ERRORLEVEL 1 EXIT /B
echo .\dist-newstyle\build\x86_64-windows\ghc-9.4.8\Haskell-0.1.0.0\x\Haskell\build\Haskell\Haskell.exe -o output.svg -w 400
.\dist-newstyle\build\x86_64-windows\ghc-9.4.8\Haskell-0.1.0.0\x\Haskell\build\Haskell\Haskell.exe -o output.svg -w 400