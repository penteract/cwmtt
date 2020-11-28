A program for analysing games of 5D chess

Can be built with the Haskell tool Stack [https://docs.haskellstack.org/en/stable/README/] by running `stack build`.
It should also be possible to build it just using Cabal.

`stack exec cwmtt` will run the program if built using Stack.

At the moment, the game is presented via a cli. There are 2 commands:
- `cwmtt play` displays the state of the game then waits for an input move.
- `cwmtt convert` reads an input of the format described in [notation.md] and outputs in Shad Amethyst's notation [https://github.com/adri326/5dchess-notation].
