module Game.Chess.TimeTravel.Layouts(standard, sb, getGame,makeFrom, layouts)
where

import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Parser(Parser, (>>$), ($$), (<?>), ret, get, parsePiece)

import Data.List
import Data.Map(Map,findWithDefault)
import Data.Maybe
import Data.Char

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

jpb = "ppppk/5/5/5/KPPPP"

makeFrom :: Board -> Player -> State
makeFrom b White = (0,[(1,[b])],[(0,[])],White)
makeFrom b Black = (0,[(1,[])],[(1,[b])],Black)

standard :: State
standard = (0,[(1,[sb])],[(0,[])],White)
halfReflected :: State
halfReflected = (0,[(1,[hrb])],[(0,[])],White)
turnZero :: State
turnZero = (0,[(1,[sb])],[(0,[sb])],White)
justPawns :: State
justPawns = single "ppppk/5/5/5/KPPPP"
justBrawns :: State
justBrawns = single "wwwwk/5/5/5/KWWWW"

single :: String -> State
single s =  (0,[(1,[fromFEN s])],[(0,[])],White)


layouts =
  [ ("Standard",standard)
  , ("Standard - Half Reflected",halfReflected)
  , ("Standard - Turn Zero", turnZero)
  , ("Focused - Just Pawns", justPawns)
  , ("Focused - Just Brawns", justBrawns)
  , ("Focused - Just Kings", single "2k/3/K2")
  ]

getGameFromString :: String -> State
getGameFromString = fromMaybe standard . flip lookup layouts

getGame :: Map String String -> State
getGame mp = fromMaybe standard (lookup (findWithDefault "" "Board" mp) layouts)

fromFEN :: String -> Board
fromFEN s = case parseBoard s of
  Just (b, "") -> transpose (reverse b)
  _ -> error "parse failed"

parseBoard :: Parser Board
parseBoard = parseRow >>$ (\ r -> (r:) $$ parseBoard')
parseBoard' = get "/" () >>$ const parseBoard  <?> ret []

parseRow :: Parser [Cell]
parseRow = parseCells >>$ (\c -> (c++) $$ parseRow)  <?> ret []

parseCells :: Parser [Cell]
parseCells (c:rs)
   | c `elem` ['0'..'9'] = Just (replicate (ord c - 48) Empty ,rs)
   | otherwise      = (parsePiece >>$ (\ p -> ret [Full (if isUpper c then White else Black, p, Still)])) (toUpper c : rs)
parseCells "" = Nothing
