module Game.Chess.TimeTravel.Printing
where

import Game.Chess.TimeTravel.Datatypes

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.Moves


drawState :: Game -> State -> String
drawState ((b:_),_) (lastTl, wtls, btls, pl) =
  let maxturn = maximum [fst t | t <- wtls]
      timelines = zipWith (drawtlpair (length b) (2*maxturn)) wtls btls in
      unlines (horizontalConcat timelines)


horizontalConcat :: [[String]] -> [String]
horizontalConcat (xs:xss) = foldr1 (zipWith (\x end -> x ++ " | " ++ end)) (xs:xss)


-- white timelines black timelines
drawtlpair :: Int -> Int -> Timeline -> Timeline -> [String]
drawtlpair boardsize maxturn (tw,wbs) (tb,bbs) =
  let boards = reverse$ if tw==tb+1 then (interleave wbs bbs)
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

showAlg :: Piece -> String
showAlg Pawn = ""
showAlg p = show p

displayMove :: State -> Move -> String
displayMove s@(_,_,_,col) (((l,t,x,y),(l',t',x',y')):rest) =
  let Just (Full (_,p,_)) = getAt s (l,t,x,y)
    in if p==Rook && not (null rest) then displayMove s rest else
      let jumpInfo = if (l,t)==(l',t') then "" else
                     (if isJust (getBoard (flipPlayer s) (l',nextT t' col)) then
                       ">>" else ">")++showLT (l',t')
        in
          showLT (l,t) ++ showAlg p ++ showPos (x,y) ++ jumpInfo ++ showPos (x',y')


showLT :: (Int,Int) -> String
showLT (l,t) = "("++show l ++ "LT"++show t++")"

showPos :: (Int,Int) -> String
showPos (f,r) = toEnum (f+fromEnum 'a'):show(r+1)
