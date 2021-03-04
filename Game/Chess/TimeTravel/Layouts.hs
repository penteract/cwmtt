module Game.Chess.TimeTravel.Layouts(standard, sb, getGame)
where

import Game.Chess.TimeTravel.Datatypes

import Data.List
import Data.Map(Map,findWithDefault)
import Data.Maybe

sr = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]
e8 = [Empty | n <- [0..7]]


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

hrb = transpose [
      map (\ p -> Full (White,p,Still)) sr
    , [Full (White,Pawn,Still) | n <- [0..7]]
    , e8
    , e8
    , e8
    , e8
    , [Full (Black,Pawn,Still) | n <- [0..7]]
    , map (\ p -> Full (Black,p,Still)) (reverse sr)
    ]

standard :: Game
standard = ((0,[(1,[sb])],[(0,[])],White), (){-[((4,y),(dx,0)) | dx <- [-1,1], y <- [0,7]]-})
halfReflected :: Game
halfReflected = ((0,[(1,[hrb])],[(0,[])],White), (){-[((4-(y`div`7),y),(dx,0)) | dx <- [-1,1], y <- [0,7]]-})
turnZero :: Game
turnZero = ((0,[(1,[sb])],[(0,[sb])],White), (){-[((4,y),(dx,0)) | dx <- [-1,1], y <- [0,7]]-})

layouts =
  [ ("Standard",standard)
  , ("Standard - Half Reflected",halfReflected)
  , ("Standard - Turn Zero", turnZero)
  ]

getGame :: Map String String -> Game
getGame mp = fromMaybe standard (lookup (findWithDefault "" "Board" mp) layouts)
