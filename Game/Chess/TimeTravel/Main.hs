
-- My pet hypothesis (which is almost certainly wrong): The perfect game of 2D chess is identical to the perfect game of 5D chess (which never branches), it's just that it's easier to find refutations in 5D.

import Game.Chess.TimeTravel.Parser
import Game.Chess.TimeTravel.PGNParser
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Layouts
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves
import Game.Chess.TimeTravel.BuildGame
import Game.Chess.TimeTravel.Utils

import System.Exit
import System.Environment
import System.IO
import Data.List
import Control.Monad
import Control.Arrow((>>>))

commands = [
   ("play",play)
  ,("convert",convert)
  ,("test",test)
  ,("debug",debug)
  ]

main = do
  args <- getArgs
  unless (null$ intersect ["-h","help","--help"] args)
    (usage >> exitSuccess)
  mapM_ (\ (name,command) ->
     when (args==[name])
       (command>>exitSuccess)) commands
  usage>>exitFailure

  --putStr$ drawState standard (1,[(2,[sb,sb]),(1,[sb])],[(1,[sb]),(0,[])],White)

usage :: IO ()
usage = putStr $ unlines[
   "usage: cwmtt <command>"
  ,"commands:"
  ,"help - print this message"
  ,"play - enter moves and see the board state after each"
  ,"convert - convert moves from my notation to Shad's"
  ]

play = do
  let s1 = (0,[(1,[sb])],[(0,[])],White)
  loop 1 standard s1 True

-- |
loop :: Int -> Game -> State -> Bool -> IO ()
loop n g s b = do
  when b $ putStr$ drawState g s
  input<-getLine
  let ta = putStrLn "try again:">>loop n g s False
  let k=parseTurn (preproc input)
  print k
  if input=="mate?" then
    let lms = legalMoveSets s in
      case lms of
        [] -> putStrLn "Checkmate!"
        (m:ms) -> putStrLn (displayMoveSet s m) >> ta
    else
      case k of
        Just (msps,"") -> case concretize s (n,msps) of
          Left e -> putStrLn e >> ta
          Right (s', mvs) -> loop (n+1) g s' True
        Just (_,rs) -> putStrLn ("Incomplete Parse - next chars:"++show rs)>> ta
        Nothing -> putStr "unable to parse move, " >> ta

  -- let lms = legalMoveSets s
  -- putStrLn$unlines $ zipWith (\ n m -> show n ++ ". " ++ intercalate ";" (map (displayMove s) m) ) [0..] lms
  -- putStr "Enter a move number:"
  -- hFlush stdout
  -- n <- readLn
  -- let s2 = apply (lms!! n) s
  -- loop (n+1) g s2 True

convert :: IO ()
convert = interact (
  -- parse
      preproc >>> getTurnSequence
  -- check and calculate the details
  >>> rDelistify (build standard)
  -- reorganize to work with 'displayMoveSet'
  >>> rToListWarn >>> (\l -> zipWith (\ (s,_) (_,m) -> (s,m)) l (tail l))
  --print
  >>> map (uncurry displayMoveSet)
  >>> zipWith (\ (col,t) m -> show t ++show col ++ ". "++m)
      (interleave (zip (repeat White) [1..]) (zip (repeat Black) [1..]))
  >>> unlines)


test :: IO ()
test = do
  let Just (pm,"") = parseTurn "Nf3"
  putStrLn""
  print pm
  case concretize (makeState (fst standard)) (1,pm) of
    Left e -> putStrLn ("err"++e)
    Right (s,ms) -> putStrLn ("move:"++displayMoveSet s ms)

{-
getAll :: IO String
getAll = do
  s <- getLine
  iseof <- isEOF
  if iseof then return ""
    else ((s++).('\n':)) <$> getAll -}

fn (Just (Notated _ ms rs, _)) = up rs (foldr Cons Nil ms)

debug :: IO ()
debug = interact (
  -- parse
    parsePGN >>> fn
  -- check and calculate the details
  >>> rDelistify (build standard)
  -- reorganize to work with 'displayMoveSet'
  >>> rToListWarn >>> (\l -> zipWith (\ (s,_) (_,m) -> (s,m)) l (tail l))
  --print
  >>> map (uncurry displayMoveSet)
  >>> zipWith (\ (col,t) m -> show t ++show col ++ ". "++m)
      (interleave (zip (repeat White) [1..]) (zip (repeat Black) [1..]))
  >>> unlines)
