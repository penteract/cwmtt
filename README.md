A program for analysing games of 5D chess

Can be built with the Haskell tool Stack [https://docs.haskellstack.org/en/stable/README/] by running `stack build`.
It should also be possible to build it just using Cabal.

`stack exec cwmtt` will run the program if built using Stack.

At the moment, the game is presented via a cli. There are 2 commands:
- `cwmtt play` displays the state of the game then waits for an input move.
- `cwmtt convert` reads an input of the format described in [notation.md] and outputs in Shad Amethyst's notation [https://github.com/adri326/5dchess-notation].

Building
==========

Prerequisites
--------------

- z3 can be found at https://github.com/Z3Prover/z3/releases/ . If you're on Linux, a package manager may be a better way to install it.

- A Haskell build tool. The instructions below assume stack https://docs.haskellstack.org/en/stable/README/ .

Building
----------
`stack build` (run from inside this repo) should download the Haskell dependencies (including a compiler and the z3 bindings) and build the program. If you don't have z3 installed, you'll get an error here. If you're on Windows, you need to add the options `--extra-include-libs=<path>\include` `--extra-bin-libs=<path>\bin` where `<path>` is the directory of the z3 download. You also need to add `<path>\bin` to the `PATH` environment variable before running the following.

`stack exec cwmtt` should then run the executable and print a usage message. The program communicates via stdin/stdout.

`cat tests/standard.5dpgn | stack exec cwmtt fastmate` will tell you whether the game described in `tests/standard.5dpgn` is checkmate. (This works in bash, I don't know what powershell does with pipes)
