module Game.Chess.TimeTravel.FastCheckMate
where
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe

import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.Moves

-- List of hypercuboids. Indices should be comparable across hypercubes
type HCs a = [HC a]

-- A hypercuboid is stored as a list of axes where each axis is a list of numbered items
type HC a = [[(Int,a)]]

-- Cross section. This is a subhypercuboid defined by a list of axes and subsets of each axis in the list.
-- For axes not mentioned, the whole axis is implied.
type XSec = [(Int,[Int])]


-- Consider using indicator functions rather than lists of integers
data Sec = XSec XSec | XmatchesOneY (Int,[Int]) [(Int,[Int])]

-- cut :: HCs a -> HC a -> XSec ->HCs a
-- cut hc (sec:xsec)=
--   let (withsec,withoutsec) = split hc sec in
--     in cut withsec xsec ++ [withoutsec]
-- cut hc [] = []

remove :: HC a -> Sec -> HCs a
remove hc (XSec xsec) = cut hc xsec
remove hc (XmatchesOneY leave jumps) =
  let (leaving, notleaving) = split hc leave
      exactlyOne nojumps [] acc = acc -- could be optimized
      exactlyOne nojumps (j:js) acc =  let (withJ,noJ) = split nojumps j  in
         exactlyOne noJ js (withJ:map (snd.flip split j) acc)
  in foldl' (\ hc j -> snd (split hc j)) notleaving jumps -- No leave and no jumps there
       : exactlyOne leaving jumps []

-- remove a cross section from a hypercuboid
cut :: HC a -> XSec -> HCs a
cut = cut' []
-- helper function
cut' :: HCs a -> HC a -> XSec ->HCs a
cut' acc hc (sec:xsec)=
  let (withsec,withoutsec) = split hc sec
    in cut' (withoutsec:acc) withsec xsec
cut' acc hc [] = acc

split :: HC a -> (Int,[Int]) -> (HC a, HC a)
split (ax:axs) (0,l) =
  let (within,outside) = partition (\ (x,y)-> x`elem`l) ax in
    (within:axs,outside:axs)
split (ax:axs) (n,l) = let (withsec,withoutsec)=split axs (n-1,l) in
  (ax:withsec,ax:withoutsec)

data AxisLoc =
    Phy Board Move -- full move data for a physical move (within one board)
  | Jump Board Coords Coords -- (origin of creating piece) (landing location (not counting jump))
  | Leave Board Coords -- origin of leaving piece -- (could just use (x,y) but this is easier)
  | Pass (Int,Int) -- l,t of passing board
   deriving (Eq)

isLeave (Leave _ _) = True
isLeave _ = False
isJump Jump {} = True
isJump _ = False
isPass (Pass _) = True
isPass _ = False

jumpSource :: AxisLoc -> Maybe Coords
jumpSource (Jump _ src _) = Just src
jumpSource _ = Nothing
jumpDest :: AxisLoc -> Maybe Coords
jumpDest (Jump _ src _) = Just src
jumpDest _ = Nothing

leaveSource :: AxisLoc -> Maybe Coords
leaveSource (Leave _ c)= Just c
leaveSource _ = Nothing

mBoard :: AxisLoc -> Maybe Board
mBoard (Phy b _) = Just b
mBoard (Jump b _ _) = Just b
mBoard (Leave b _) = Just b
mBoard (Pass _) = Nothing

data Info = Info{
     state :: State
   , numPlayable :: Int
   , lMap :: [(Int,Int)] -- maps L index to axis
 }

-- Should be much faster at telling if something is checkmate.
fastLegalMoveSets :: State -> [MoveSet]
fastLegalMoveSets s =
  uncurry search (buildHC s)

-- reasons a moveset might be illegal:
-- A king can be captured
-- it doesn't move the present
-- There might not be a consistent ordering of branches:
--   there isn't a 'Jump' matching a 'Leave'
--   an earlier (new) timeline might have a pass while a later one gets created (This can be eliminated)
--   the order of "branches that could be hops" might not be consistent (an earlier created timeline involves jumping to a playable board that creates a later timeline)
--   -- There might be a cycle of "branches that could be hops"
--     -- (not a concern because it falls within the previous case; might become a concern if optimized so that newly created timelines aren't distinguished by origin)


-- A lot of the above could be dealt with by cutting up the hypercuboid before starting
-- I don't believe that should be done because it might be faster to rule out everyting else before doing that.

-- ( axis {-maps to L-},(index, move))
type Cub = [(Int,(Int,AxisLoc))]

search :: Info -> HCs AxisLoc -> [MoveSet]
search inf@(Info s nP lmp) (hc:hcs) =
  case mapM listToMaybe hc of
    Nothing -> search inf hcs
    Just c ->
      let cell = zip [0..] c
          mvset = makeMoveset (map (snd.snd) cell)
          newState = apply mvset s
          xsecs = findProblems inf cell newState hc
          in
        case xsecs of
          [] -> mvset : search inf (sanecut inf hc
                  (XSec $ zipWith (\ (a,b) i -> (i,[a])) cell [0..]) ++ hcs)
          (xsec:_) -> search inf (sanecut inf hc xsec ++hcs)
search inf [] = []

sanecut :: Info -> HC AxisLoc -> Sec -> [HC AxisLoc]
sanecut inf hc xsec = remove hc xsec >>= sanity inf

-- Check that if a new timeline must be created (it cannot be 'pass'), then
--
sanity :: Info -> HC AxisLoc -> [HC AxisLoc]
sanity (Info _ nP _) hc = if any null hc then [] else [res]
  where
    (playFrom,new) = splitAt nP hc
    (_,res) = foldr (\ (n:ns) (b,rest) -> (b || isPass (snd n), (if b && isPass(snd n) then ns else n:ns):rest)) (False,[]) new


findProblems :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
--findProblems numPlayable cell hc s = concat [test numPlayable cell hc s | test <- [findChecks, testPresent, jumpsMatchLeaves, jumpOrderConsistent]]
findProblems inf cell s hc =
  findChecks inf cell s hc ++
  testPresent inf cell s hc ++
  jumpsMatchLeaves inf cell s hc ++
  jumpOrderConsistent inf cell s hc

-- Check if a branch involves jumping to a source board of a branch that must be created later
jumpOrderConsistent :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
jumpOrderConsistent (Info _ nP lmp) cell s hc = sec
  where
    (jumpSrcs,jumpDests) = unzip [(((l,t),ax),((l',t'),ax))
       | (ax,(ix,loc))<- drop nP cell, Jump _ (l,t,_,_) (l',t',_,_) <- [loc]]
    conflicts = [ (lt,ax,ax')
                 | ((lt,ax),rest) <- zip jumpDests (tail$ tails jumpSrcs),
                     (lt',ax') <- rest, lt==lt' ]
    sec = [XSec [filterAxis (or.((==lt).fstPair<$>).jumpDest.snd) ax hc,
                 filterAxis (or.((==lt).fstPair<$>).jumpSource.snd) ax' hc]
           | (lt,ax,ax') <- conflicts]



filterAxis :: ((Int,a) -> Bool) -> Int -> HC a -> (Int,[Int])
filterAxis f ax axes = (ax, map fst $ filter f (axes !! ax))

jumpsMatchLeaves :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
jumpsMatchLeaves (Info _ nP lmp) cell s hc = noDups ++ srcMatches ++ mustAppear
  where
    jumps = -- cell >>= (\ cl@(ax,(ix,loc)) -> map (\ src -> (src,cl)) (maybeToList (jumpSource loc)))
      [(src,cl) | cl@(ax,(ix,loc))<- cell, Just src <- [jumpSource loc]]
    -- No 2 jumps have the same source position
    makeSec :: Coords -> Sec
    makeSec c@(l,_,_,_) = let
      Just ax = lookup l lmp in
        XmatchesOneY (ax,[ ix | (ix,loc)<-hc!!ax, leaveSource loc == Just c])
          [(ax', [ ix | (ix,loc) <-locs, jumpSource loc==Just c]) | (ax',locs) <- zip [0..] hc, ax'/=ax]
    sortedJumps = sortOn fst jumps
    noDups = [ makeSec c |
        ((c,(ax,_)),(c',(ax',_))) <- zip sortedJumps (tail sortedJumps), c==c']
    -- if there is a jump from l, the right piece must leave l
    srcMatches = [ makeSec c |
       (c@(l,_,_,_),(ax,_)) <- jumps,
       ax' <- [fromJust $ lookup l lmp], Just c /= (leaveSource.snd.snd) (cell!!ax')
      ]
    -- Each leave must appear as some jump
    mustAppear = [ makeSec c |
      (Just c)<- map (leaveSource.snd.snd) cell, c `notElem` map fst jumps
     ]
    {-
    -- No 2 jumps have the same source l
    noDups = [ [filterAxis (or.(((==l).fst4<$>).jumpSource.snd)) ax hc,
                filterAxis (or.(((==l).fst4<$>).jumpSource.snd)) ax' hc]
        | (((l,_,_,_),(ax,_)),((l',_,_,_),(ax',_))) <- zip sortedJumps (tail sortedJumps), l==l']
    -- if there is a jump from l, the right piece must leave l
    srcMatches = [[
        filterAxis (or.(((== c)<$>).jumpSource.snd)) ax hc
        , filterAxis (and.(((/=c)<$>).leaveSource.snd)) ax' hc
        ]
       | (c@(l,_,_,_),(ax,_)) <- jumps, ax' <- [fromJust $ lookup l lmp], Just c /= (leaveSource.snd.snd) (cell!!ax')
      ]
    -- Each leave must appear as some jump
    mustAppear = [
      |
      (Just c)<- map (leaveSource.snd.snd) cell,
      ]-}

-- This will want optimising: there must be a better way to eliminate things that don't pass the present to the opponent
-- idea: put pass at the end of non-branching axes
testPresent :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
testPresent (Info s@(nw,wtls,btls,col) nP lmp) cell newS hc = secs
  where
    -- Work out which set of new tls can move time
    (always,newBoards) = splitAt nP cell
    nb = length wtls - nw - 1
    (nMine,nOther) = if col==White then (nw,nb) else (nb,nw)
    nPri = nOther + 1 - nMine -- how many active tls can be created?
    nA = numActive s
    (tMovers,rest) = break (isPass.snd.snd) (take nPri newBoards)
    rs = case rest of
      [] -> []
      ((ax,(ix,Pass _)):_) -> [(ax,[ix])]

    -- activatable = map fst (take nPri newBoards)
    -- check how boards that can be activated affect this

    -- Consider adding sanity checks that newS matches our calculations
    --   -- null secs == correctTurn newS

    newt = present newS
    -- see if there are any passes on active boards before the present
    secs = [ XSec ((ax,[ix]):rs++[ filterAxis (\ (_,Jump _ _ (_,dt,_,_))-> dt>=newt) ax hc | (ax,(ix,loc))<-tMovers])
      | (ax,(ix,Pass (l,t))) <- always , t <= newt && abs l<=nA+length tMovers ] -- if we overshoot, all relevant TLs are active



findChecks :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
findChecks (Info oldS _ lmp) cell newS@(_,_,_,playerCol) hc = do
  (l,t) <- playableBoards newS
  --let Just b = getBoard (upi (putStrLn (drawState standard s) >> print lt >> return s)) (l,t)
  let Just b = getBoard newS (l,t)
  (file,x) <- zip b [0..]
  (cl@(Full (pieceCol,piece,_)),y) <- zip file [0..]
  True <- return (pieceCol==playerCol)
  let pos = (l,t,x,y)
      dirmoves = do
        d <- directions piece
        let ln = line pos d (getAt newS)
        Just (pos',Full (col,King,_)) <- [foldr (\ x y -> y <|> Just x) Nothing ln]
        if col /= playerCol
          then return (fromCells oldS ((pos,cl):ln) hc lmp)
          else []
      fixedmoves = do
        d <- fixed piece ++ fixedCapturing pieceCol piece
        case getAt newS (pos + d) of
          Just c@(Full (col,King,_)) ->
            if col /= playerCol
              then return (fromCells oldS [(pos,cl),(pos+d,c)] hc lmp)
              else []
          _ -> []
  fixedmoves ++ dirmoves

-- Take the cross section in which the given cells have the given values
fromCells :: State -> [((Int, Int, Int, Int), Cell)] -> HC AxisLoc -> [(Int, Int)] -> Sec
fromCells s pcs hc lmp = XSec [filterAxis
   (\ (_, j) -> Just cell == (mBoard j >>= flip getAtBoard (x, y))) ax
   hc |
   (pos@(l, t, x, y), cell) <- pcs,
   fromMaybe t (getTime s l) == t,
   Just ax <- [lookup l lmp]]


makeMoveset :: [AxisLoc] -> MoveSet
makeMoveset = (>>= toMove) -- branches are already at the end
  where
    toMove :: AxisLoc -> [Move]
    toMove (Pass _) = []
    toMove (Leave _ _) = []
    toMove (Phy _ mv) = [mv]
    toMove (Jump _ src dest ) = [[(src,dest)]]

buildHC :: State -> (Info, HCs AxisLoc)
buildHC s =
  let pbs = playableBoards s
      bmts = [(l,[(m,getType pbs m) | m <- []:legalMovesFromBoard s pb ])
                | pb@(l,t) <- pbs]
      in makeAxes s bmts


-- Consider doing some sorting
makeAxes :: State -> [(Int,[(Move,MoveType)])] -> (Info, HCs AxisLoc)
makeAxes s lmvs =
  let newL = getNewL s
      axisParts = map (toAxes s newL) lmvs
      build (n,_) (ax,_) = zip [0..] (ax ++ [ loc | (_,laxs)<-axisParts, (m,loc)<-laxs, m==n])
      maxBranches = length$ filter (any isLeave . fst) axisParts
      newBranches = Pass undefined : [ loc | (_,laxs)<-axisParts, (m,loc)<-laxs, m==newL]
      nP = length lmvs
      inf = Info s nP (zip (map fst lmvs ++ [newL,newL+signum newL .. newL+maxBranches-1]) [0..])

    in (inf, [zipWith build lmvs axisParts ++ [zip [0..] newBranches | _ <- [1..maxBranches]]])

-- Turn a list of moves into an axis + some moves for other axes
toAxes :: State -> Int -> (Int,[(Move,MoveType)]) -> ([AxisLoc], [(Int,AxisLoc)])
toAxes s newL (oldL,mvs) =
  let axls = mvs >>= \ mty -> toLoc s mty oldL newL
  in first (map snd) (partition (\x -> fst x==oldL) axls)

-- given a move, begin making it part of an axis
toLoc :: State -> (Move,MoveType) -> Int -> Int -> [(Int,AxisLoc)]
toLoc s (m,NoAct) oldL newL = [(oldL,Pass (oldL, fromJust $ getTime s oldL))]
toLoc s (m,ty) oldL newL = -- I wish I had a reason to write f oldL
  let newboards = foldl' (fullMove' s) [] m in
    newboards >>= (\ ((l,t),b) ->
      case ty of
        SameBoard -> [(l,Phy b m)]
        Hop landing -> if l==oldL then [(l, Leave b (fst$head m))] else
          [(landing, uncurry (Jump b) (head m)), (newL, uncurry (Jump b) (head m))]
        Branch ->if l==oldL && Just t==getTime s l then [(l, Leave b (fst$head m))] else
          [(newL, uncurry (Jump b) (head m))]
        _ -> error ("bad movetype"++show ty++"L:"++show oldL++";"++show l ))

-- Optimization: consider cases where the present might move separately
-- For determining checkmate, it is unnecessary to consider
-- new TLs created later than the one which moves the present
-- (note:if there are inactive timelines, don't forget about them being activated)
