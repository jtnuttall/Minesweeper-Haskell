# Haskell Minesweeper

- Minesweeper clone in Haskell

- Uses the Gtk3 binding from Hackage

- To use:
	1. Select a grid size by entering rows and colums into the entries

	2. Press `enter` or `Run`

	3. Play minesweeper

	4. To start a new game, press `Run` again

## Compile
- I suggest you create a sandbox first

    cabal sandbox init

- Get the dependencies for the program

    cabal -j --only-dependencies

- Build the program

    cabal build

You can change the option `-with-rtsopts=-N` under `ghc-options` in `Minesweeper.cabal`
to `-with-rtsopts-Nx` where `x` is the number of cores on the machine to get the parallel
garbage collector.

## Run
`cabal run`
