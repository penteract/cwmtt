module Game.Chess.TimeTravel.Printing
where

import Game.Chess.TimeTravel.Datatypes

import Data.Char
import Data.List
import Control.Monad
import Game.Chess.TimeTravel.Utils


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
