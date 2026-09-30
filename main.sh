#!/usr/bin/env bash
set -euo pipefail

# GTK loads src-exe/Flow1.glade relative to the project root.
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
exec cabal v2-run exe:haskell -- -o ./src-exe/output.svg -w 400 "$@"
