{-# LANGUAGE BlockArguments #-}

module Game.Chess.TimeTravel.BuildGame
where
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves
import Game.Chess.TimeTravel.Parser
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Utils

import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.List
import Data.Maybe

-- | generate a game from a parsed input.
--buildGame :: Game -> [MoveSetPartial] -> Either String [State]
--buildGame g@(initialboards,_) mvsps = rEToList $ getGame g mvsps

buildGame :: Game -> [MoveSetPartial] -> Result String State
buildGame g@(initialboards,_) mvsps =
  rscanl applyPartialMoveSet (makeState initialboards) (zip [1..] mvsps)


makeState :: [Board] -> State
makeState bs = (0, [(1,[b])|b <- bs], [(0,[])|b<-bs], White)

applyPartialMoveSet :: State -> (Int, MoveSetPartial) -> Either String State
applyPartialMoveSet s m = fst <$> concretize s m

build :: Game -> [MoveSetPartial] -> Result String (State,MoveSet)
build g@(initialboards,_) msps =
  rscanl (concretize . fst ) (makeState initialboards,undefined) (zip [1..] msps)


getSourceL :: Move -> Int
getSourceL (((l,_,_,_),_):_) = l

-- | apply an entered moveset to a state
-- if successful, returns the new state and a precise specification of the moveset
concretize :: State -> (Int,MoveSetPartial) -> Either String (State,MoveSet)
concretize s@(nw,wbs,bbs,pl) (m, MSP n t p mvps c) = do
  "Move number " +? testIs n ((m+1) `div` 2)
  "Player " +? testIs p pl
  "Present " +? testIs t (present s)
  (mvs, arrivals) <- concretizeMoves s (min nw (numActive s)) mvps
  let ls = map (getSourceL.fst) mvs
  hasdup ls ?-> (\ l -> "more than 1 move from timeline "++show l)
  let (branches,nonbranch) = partition ((==Branch).snd) mvs
  let hoptargets = [l | (_,Hop l) <- nonbranch]
  hasdup (ls ++ hoptargets) ?-> (\ l -> "more than 1 hop to timeline "++show l)
  mapM_ (\ x -> unless (x`elem`hoptargets) (Left ("No hop to timeline "++show x)))
        arrivals
  let nonbranchs = map fst nonbranch
  (s', mvs) <- resolveBranches (foldl' fullMove s nonbranchs) (map fst branches)
  let s'' = flipPlayer s'
  unless (correctTurn s'') (Left "Did not move in all active present timelines")
  when (isKnownCheck s'' Nothing) (Left "In check")
  return (s'',mvs++nonbranchs)

hasdup :: Ord a => [a] -> Maybe a
hasdup xs = let xs' = sort xs in
  case [ x | (x,y) <- zip xs' (tail xs') , x==y] of
    (x:_) -> Just x
    [] -> Nothing

-- repeatedly apply the first move that is branching
resolveBranches :: State -> [Move] -> Either String (State,MoveSet)
resolveBranches s [] = Right (s,[])
resolveBranches s mvs = do
  let pbs = playableBoards s
  let (nonbrs, rest) = span (\((_,(l,t,_,_)):_) -> (l,t)`elem`pbs) mvs
  case rest of
    [] -> Left ("Unable to resolve branches: conflicts among"++displayMoveSet s mvs)
    (x:xs) -> second (x:) <$> resolveBranches (fullMove s x) (nonbrs++xs)

-- Given partial move data, produce the concrete moves
concretizeMoves :: State -> Int -> [MoveP] -> Either String ([(Move,MoveType)],[Int])
concretizeMoves _ _ [] = Right ([],[])
concretizeMoves s expectedl (MoveFrom m :ms) = do
  let MD p src@(sl,st,sx,sy) jt caps (dl,dt,dx,dy) (lrel,trel) = m
  let l = fromMaybe expectedl sl
  let pbs = playableBoards s
  t <- lookup l pbs ? "Not a playable timeline: "++show l
  "Source T-index " +? testIs st t
  let dt' = (if trel then (t+) else id) <$> dt
  let dl' = (if lrel then (l+) else id) <$> dl
  let candidates = movesFromBoard s (l,t)
        & filter (any \ (spos,dpos) ->
             matches spos src
          && matches dpos (dl',dt',dx,dy)
          && getPiece (getAt s spos) == p)
        -- Castling is not considered a rook move
        & filter (\mvs -> p /= Just Rook || length mvs == 1)

  candidates <- case jt of
    SingleBoard -> do
      "T-index " +? testIs dt' t
      "L-index " +? testIs dl' l
      return$ filter (any \(_,(l',t',_,_)) -> l'==l && t'==t) candidates
    Hopping -> return$
      filter (any \(_,(l',t',_,_)) -> l'/=l && (l',t')`elem`pbs) candidates
    Branching -> return$
      filter (any \(_,(l',t',_,_)) -> (l', t')/=(l,t)) candidates

  mv <- case candidates of
    [] -> Left ("s:"++show (l,t,sl,st) ++" d:"++ show (dl',dt',dx,dy)++ " No matching moves")
    [x] -> Right (x, if jt==Branching then Branch else getType pbs x)
    (x:y:_) -> Left ("Ambigous: "++displayMove s x++" or "++displayMove s y++"...")
  first (mv:) <$> concretizeMoves s (l-1) ms
concretizeMoves s l (other : ms) =
  (case other of
    Arrive -> second (l:)
    Pass -> id
    NotTurn -> id ) <$> concretizeMoves s (l-1) ms

matches :: Coords -> PartialPos -> Bool
matches (a,b,c,d) (ma,mb,mc,md) = and$
  zipWith (\ x my -> maybe True (==x) my ) [a,b,c,d] [ma,mb,mc,md]

getPiece :: Maybe Cell -> Maybe Piece
getPiece (Just (Full (_,p,_))) = Just p
getPiece _ = Nothing

-- | checks that if a piece of data is present it is the expected value
testIs :: (Eq a, Show a) => Maybe a -> a -> Either String ()
testIs Nothing _ = Right ()
testIs (Just x) y = if x==y then Right ()
  else Left ("expected "++show y++" found "++show x)
