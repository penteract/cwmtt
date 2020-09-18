
-- My pet hypothesis (which is almost certainly wrong): The perfect game of 2D chess is identical to the perfect game of 5D chess (which never branches), it's just that it's easier to find refutations in 5D.

import Game.Chess.TimeTravel.Parser
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Layouts
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves

import System.IO

main = do
  --interact (show . getTurnSequence . preproc)
  --putStr$ drawState standard (1,[(2,[sb,sb]),(1,[sb])],[(1,[sb]),(0,[])],White)
  let s1 = (0,[(1,[sb])],[(0,[])],White)
  loop standard s1

  --putStr$ drawState standard (1,[(2,[sb,sb]),(1,[sb])],[(1,[sb]),(0,[])],White)


loop g s = do
  putStr$ drawState g s
  let lms = legalMoveSets s
  putStrLn$unlines $ map show (zip [0..] lms)
  putStr "Enter a move number:"
  hFlush stdout
  n <- readLn
  let s2 = apply (lms!! n) s
  loop g s2
