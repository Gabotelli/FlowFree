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
cabal v2-run exe:haskell -- -o ./src-exe/output.svg -w 400
```

Both launch scripts now use `cabal v2-run exe:haskell -- -o ./src-exe/output.svg -w 400`, so Cabal locates/builds the executable declared in `Haskell.cabal`. They first switch to the project root because GTK loads `src-exe/Flow1.glade` relative to that directory. Additional command-line arguments are forwarded. Run either with `bash main.sh` or `bash run.sh`.

Shell syntax and root-directory/argument forwarding have been checked. GHC, Cabal and GTK are not installed in the verification environment, so a clean application build/run remains unverified. Tracked `dist-newstyle/` build artifacts have been removed, and all directories with that name are ignored.

In the interface, select a board file to solve it, or enter color numbers into the cells of one of the playable boards and use the check control. See the [menu](src-exe/menu.png) and [solver view](src-exe/resolver.png).
