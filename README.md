# FlowFree: SAT-based puzzle solver

A desktop application for solving FlowFree boards and playing three included boards. The solver encodes the puzzle as a Boolean satisfiability (SAT) problem and uses the Haskell solver [Surely](https://github.com/gatlin/surely). The encoding was inspired by [Matt Zucker's discussion of Flow Free and SAT](https://mzucker.github.io/2016/09/02/eating-sat-flavored-crow.html).

This is a joint project by **Gabriel Suárez and Ángela Gutiérrez**. The repository contains the Haskell application and board parser, a GTK interface, and C resources. It does not establish a separate authorship breakdown for individual components.

## Structure

- `src-exe/Main.hs`: application logic, board handling and GTK interface.
- `src-exe/Surely.hs`: SAT solver code used by the application; credit belongs to the upstream Surely project.
- `src-exe/Flow1.glade`: GTK interface definition.
- `csrc/resources.c`: compiled UI resources.
- `Haskell.cabal`: package dependencies and executable definition.
- `src-exe/ejemploTablero.png`: illustration of the board input format.

## Build and run

The project uses Cabal and GTK development libraries. From the repository root, try:

```bash
cabal v2-build
cabal v2-run haskell -- -o ./src-exe/output.svg -w 400
```

The included `main.sh` and `run.sh` call a build output under a hard-coded Linux/GHC 8.8.4 path. They may need adaptation on another machine; the commands above reflect the Cabal executable declared in `Haskell.cabal` and have not been verified on a clean system. Tracked `dist-newstyle/` build artifacts have been removed, and all directories with that name are now ignored. The launch scripts remain unchanged pending a verified portability update.

In the interface, select a board file to solve it, or enter color numbers into the cells of one of the playable boards and use the check control. See the [menu](src-exe/menu.png) and [solver view](src-exe/resolver.png).
