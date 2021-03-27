A program for analysing games of 5D chess

At the moment, the game is presented via a cli. There are 2 commands:
- `cwmtt play` displays the state of the game then waits for an input move.
- `cwmtt convert` reads an input of the format described in [notation.md](notation.md) and outputs in [5dpgn](https://github.com/adri326/5dchess-notation).
- `cwmtt fastmate` uses z3  to determine if a game (given in the 5dpgn format) ends in checkmate. Currently(27th March 2021) this has the best worst-case performance (among test cases) of any checkmate detection program (not counting the official client which is believed to incorrectly claim some positions are checkmate).
- `cwmtt perftest` does the above for all intermediate states of a game (printing `1` when the situation is not checkmate and `0` when it is). This is primarily useful for testing performance in a variety of cases which are not checkmate.

Building
==========

Prerequisites
--------------
- z3 can be found at https://github.com/Z3Prover/z3/releases/ . If you're on Linux, a package manager may be a better way to install it.

- A Haskell build tool. The instructions below assume stack https://docs.haskellstack.org/en/stable/README/ .

Building
----------
`stack build` (run from inside this repo) should download the Haskell dependencies (including a compiler and the z3 bindings) and build the program. If you don't have z3 installed, you'll get an error here.

`stack exec cwmtt` should then run the executable and print a usage message. The program communicates via stdin/stdout.

`cat tests/standard.5dpgn | stack exec cwmtt fastmate` will tell you whether the game described in `tests/standard.5dpgn` is checkmate. (This works in bash, I don't know what powershell does with pipes)

Windows
----------
If you're on Windows, you need to run `stack build` with the options `--extra-include-libs=<path>\include` `--extra-bin-libs=<path>\bin` where `<path>` is wherever you unzipped z3. You also need to add `<path>\bin` to the `PATH` environment variable before `stack exec cwmtt` will work.
