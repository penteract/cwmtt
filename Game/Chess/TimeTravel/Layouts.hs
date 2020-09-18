module Game.Chess.TimeTravel.Layouts(standard, sb)
where

import Game.Chess.TimeTravel.Datatypes

import Data.List

sr = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
e8 = [Empty | n <- [0..7]]

standard :: Game
standard = ([transpose [
    map (\ p -> Full (White,p,Still)) sr
  , [Full (White,Pawn,Still) | n <- [0..7]]
  , e8
  , e8
  , e8
  , e8
  , [Full (Black,Pawn,Still) | n <- [0..7]]
  , map (\ p -> Full (Black,p,Still)) sr

  ]], [((4,y),(dx,0)) | dx <- [-1,1], y <- [0,7]])

sb = transpose [
      map (\ p -> Full (White,p,Still)) sr
    , [Full (White,Pawn,Still) | n <- [0..7]]
    , e8
    , e8
    , e8
    , e8
    , [Full (Black,Pawn,Still) | n <- [0..7]]
    , map (\ p -> Full (Black,p,Still)) sr

    ]
