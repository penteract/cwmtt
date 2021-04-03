module Game.Chess.TimeTravel.Printing
where

import Game.Chess.TimeTravel.Datatypes

import Data.Char
import Data.List
import Data.Maybe
import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.Moves


drawState :: State -> String
drawState (lastTl, wtls, btls, pl) =
  let someboard = head $ concat (map snd (wtls ++ btls))
      maxturn = maximum [fst t | t <- wtls]
      timelines = zipWith (drawtlpair (length someboard) (2*maxturn)) wtls btls in
      unlines (horizontalConcat timelines)


horizontalConcat :: [[String]] -> [String]
horizontalConcat (xs:xss) = foldr1 (zipWith (\x end -> x ++ " | " ++ end)) (xs:xss)


-- white timelines black timelines
drawtlpair :: Int -> Int -> Timeline -> Timeline -> [String]
drawtlpair boardsize maxturn (tw,wbs) (tb,bbs) =
  let boards = reverse$ if tw==tb+1 then interleave wbs bbs
          else if tw==tb then interleave bbs wbs
          else error "misaligned most recent white and black boards"
      prefill = replicate ((boardsize+3)*(tw+tb - length boards)) (replicate boardsize ' ')
      postfill = replicate ((boardsize+3)*(maxturn - (tw+tb))) (replicate boardsize ' ') in
  prefill ++ intercalate (tsep boardsize)  (map drawb boards) ++ postfill


tsep :: Int -> [String]
tsep boardsize = [spaces, hbar, spaces]
  where spaces = replicate boardsize ' '
        hbar = replicate boardsize '-'

drawb :: Board -> [String]
drawb = map (map showc)

showc :: Cell -> Char
showc (Full p) = showp p
showc Empty = ' '

showp :: ColouredPiece -> Char
showp (White, p, _) = head $ show p
showp (Black, p, _) = toLower.head$  show p


-- Print according to Shad's notation

showAlg :: Piece -> String
showAlg Pawn = ""
showAlg p = show p

isCapture :: State -> Move -> String
isCapture s@(_,_,_,col) mv@((_,dest):rest) = case getAt s dest of
  Just (Full (c,_,_)) -> if c /= col then "x" else isCapture s rest
  _ -> isCapture s rest
isCapture s [] = ""

displayMove :: State -> Move -> String
displayMove s@(_,_,_,col) mv@(((l,t,x,y),(l',t',x',y')):rest) =
  let Just (Full (_,p,_)) = getAt s (l,t,x,y)
    in if p==Rook && not (null rest) then displayCastle rest else
      let jumpInfo = if (l,t)==(l',t') then isCapture s mv else
                     (if isJust (getBoard (flipPlayer s) (l',nextT t' col)) then
                       ">>" else ">")++isCapture s mv++showLT (l',t')
        in
          showLT (l,t) ++ showAlg p ++ showPos (x,y) ++ jumpInfo ++ showPos (x',y')

displayCastle [((l,t,x,_),(_,_,x',_))] = showLT (l,t) ++
  if x'>x
    -- kingside (on standard board)
    then "O-O"
    -- queenside (on standard board)
    else "O-O-O"

displayMoveSet :: State -> MoveSet -> String
displayMoveSet s = unwords . reverse . fst . foldl (\ (mvs, s') m -> (displayMove s' m : mvs, fullMove s' m) ) ([],s)

showLT :: (Int,Int) -> String
showLT (l,t) = "("++show l ++ "T"++show t++")"

showPos :: (Int,Int) -> String
showPos (f,r) = toEnum (f+fromEnum 'a'):show(r+1)

printPGN :: [(State,MoveSet)] -> String
printPGN = unlines . zipWith (curry displayMoveSetCol) [2..]
  where
    displayMoveSetCol (n,(s@(_,_,_,Black),mv)) = "/" ++ displayMoveSet s mv
    displayMoveSetCol (n,(s@(_,_,_,White),mv)) = show (n`div`2)++"." ++ displayMoveSet s mv
