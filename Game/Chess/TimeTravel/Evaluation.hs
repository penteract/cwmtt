module Game.Chess.TimeTravel.Evaluation
where

import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves

{- Work in progress -}

checkmateScore :: Int
checkmateScore = 10000000000


-- | 1 point per available move, 20? points per available check
scoreBoardMoves :: State -> (Int,Int) -> Int
scoreBoardMoves s@(n, wtls, btls, col) (l,t) = undefined


-- | Give a score to a board, with a particular player to move
scoreBoardPieces :: Board -> Player -> Int
scoreBoardPieces board pl = sum [
  if col==pl then value p else negate (value p) |
    file <- board,
    Full (col, p, _) <- file]

value :: Piece -> Int
value Pawn = 10
value Bishop = 50
value Knight = 45
value Queen = 120
value King = -70
value Rook = 30
