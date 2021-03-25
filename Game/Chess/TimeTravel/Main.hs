
-- My pet hypothesis (which is almost certainly wrong): The perfect game of 2D chess is identical to the perfect game of 5D chess (which never branches), it's just that it's easier to find refutations in 5D.

import Game.Chess.TimeTravel.Parser
import Game.Chess.TimeTravel.PGNParser
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Layouts
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves
import Game.Chess.TimeTravel.BuildGame
import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.FastCheckMate

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
  ,("checkmate",detectCheckmate)
  ,("fastmate",detectCheckmateFast)
  ,("perftest",detectCheckmateAll)
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
debug :: IO ()
debug =  do
  inp <- getContents
  let Just (Notated tags mvs rest, _) = parsePGN inp
      g = getGame tags
  if null rest then return ()
    else putStr "Unparsed: " >> print rest
  let ss = rToListWarn (build g mvs)
  mapM_ (\(i,(s,_)) -> do
    let lms = legalMoveSets s
    let flms = fastLegalMoveSets s
    --putStrLn (drawState g s)
    print i
    --print (length (take 1 lms))
    --mapM_ (putStrLn.displayMoveSet s) (sort $ take 1 lms)
    --print (length (nub $ sort $ take 2 flms))
    --print (length flms)
    print (length (take 1 flms))
    mapM_ (putStrLn.displayMoveSet s) (take 1 flms)
    {-when (nl/=nf)(do
    putStrLn (drawState g s)
    print nf
    let nsls = (map (\ x -> (sort x,x)) lms)
    let nsfs = (map (\ x -> (sort x,x)) flms)
    let diff = sortingDiffOn fst nsls nsfs
    --print (length nsls,length nsfs) --, length diff)
    mapM_ (putStrLn.displayMoveSet s.snd) (take 100 diff)
    mapM_ (print) (take 100 diff)
    --putStrLn (drawState g (apply (head diff) s))
    error "stop"
    )-}
    --print (lengthGT flms 100)
    putStrLn ""
    ) (zip [1..] ss)

usage :: IO ()
usage = putStr $ unlines[
   "usage: cwmtt <command>"
  ,"commands:"
  ,"help - print this message"
  ,"play - enter moves and see the board state after each"
  ,"convert - convert moves from my notation to Shad's"
  ,"checkmate - test if a situation is checkmate"
  ,"fastmate - quickly test if a situation is checkmate"
  ,"perftest - test checkmate for all intermediate positions in a game"
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
  case concretize (fst standard) (1,pm) of
    Left e -> putStrLn ("err"++e)
    Right (s,ms) -> putStrLn ("move:"++displayMoveSet s ms)

{-
getAll :: IO String
getAll = do
  s <- getLine
  iseof <- isEOF
  if iseof then return ""
    else ((s++).('\n':)) <$> getAll -}

fn (Just (Notated _ ms rs, _)) = up rs ms


lengthGT :: [a] -> Int -> Bool
lengthGT [] _ =  False
lengthGT (_:xs) 0 = True
lengthGT (_:xs) n = lengthGT xs (n-1)

detectCheckmate :: IO ()
detectCheckmate = do
  inp <- getContents
  let Just (Notated tags mvs rest, _) = parsePGN inp
      g = getGame tags
  if null rest then return ()
    else putStr "Unparsed: " >> print rest
  let ss = rToListWarn (build g mvs)
      (final,_) = last ss
      lms = legalMoveSets final
  case lms of
    [] -> putStrLn "Checkmate"
    (m:ms) -> putStrLn ("Not checkmate: " ++ displayMoveSet final m)


detectCheckmateFast :: IO ()
detectCheckmateFast = do
  inp <- getContents
  let Just (Notated tags mvs rest, _) = parsePGN inp
      g = getGame tags
  if null rest then return ()
    else putStr "Unparsed: " >> print rest
  let ss = rToListWarn (build g mvs)
      (final,_) = last ss
      lms = fastLegalMoveSets final
  case lms of
    [] -> putStrLn "Checkmate"
    (m:ms) -> putStrLn ("Not checkmate: " ++ displayMoveSet final m)


detectCheckmateAll :: IO ()
detectCheckmateAll = do
  inp <- getContents
  let Just (Notated tags mvs rest, _) = parsePGN inp
      g = getGame tags
  if null rest then return ()
    else putStr "Unparsed: " >> print rest
  let ss = rToListWarn (build g mvs)
  hSetBuffering stdout NoBuffering
  mapM_ (
    putStr.show .length .take 1 .fastLegalMoveSets.fst
    ) ss
