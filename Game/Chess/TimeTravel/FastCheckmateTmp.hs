module Game.Chess.TimeTravel.FastCheckmateTmp
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
import Game.Chess.TimeTravel.Printing (drawState,displayMoveSet)

-- List of hypercuboids. Indices should be comparable across hypercubes
type HCs a = [HC a]

size :: HCs a -> Integer
size xs = sum [product (map (toInteger.length) x) | x <- xs]

contains :: HCs a -> [Int] -> Bool
contains hcs ixs = any (flip contain ixs) hcs

-- A hypercuboid is stored as a list of axes where each axis is a list of numbered items
type HC a = [[(Int,a)]]
contain :: HC a -> [Int] -> Bool
contain [] [] = True
contain (ax:axs) (ix:ixs) = ix `elem` (map fst ax) && contain axs ixs

-- Cross section. This is a subhypercuboid defined by a list of axes and subsets of each axis in the list.
-- For axes not mentioned, the whole axis is implied.
type XSec = [(Int,[Int])]


-- Consider using indicator functions rather than lists of integers
data Sec = XSec XSec | XmatchesOneY (Int,[Int]) [(Int,[Int])] deriving (Eq,Show)


remove :: HC a -> Sec -> HCs a
remove hc (XSec xsec) = cut hc xsec
remove hc m@(XmatchesOneY leave jumps) =
  let (leaving, notleaving) = split hc leave
      exactlyOne nojumps [] acc = acc -- could be optimized
      exactlyOne nojumps (j:js) acc =  let (withJ,noJ) = split nojumps j  in
         exactlyOne noJ js (withJ:map (snd.flip split j) acc)
  in {-up m $-} foldl' (\ hc j -> snd (split hc j)) notleaving jumps -- No leave and no jumps there
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

instance Show AxisLoc where
  show (Phy _ m) = "Phy"++show m
  show (Jump _ s t) = "Jump"++show s++ show t
  show (Leave _ m) = "Leave"++show m
  show (Pass _) = "Pass"++show "unwilling to display"

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
jumpDest (Jump _ _ dest) = Just dest
jumpDest _ = Nothing

leaveSource :: AxisLoc -> Maybe Coords
leaveSource (Leave _ c)= Just c
leaveSource _ = Nothing

mBoard :: AxisLoc -> Maybe Board
mBoard (Phy b _) = Just b
mBoard (Jump b _ _) = Just b
mBoard (Leave b _) = Just b
mBoard (Pass _) = Nothing

getLTFromLoc :: AxisLoc -> (Int, Int)
getLTFromLoc (Pass lt) = lt
getLTFromLoc (Phy b (((l,t,_,_),_):_)) = (l,t)
getLTFromLoc (Jump b _ (l,t,_,_)) = (l,t)
getLTFromLoc (Leave _ (l,t,_,_)) = (l,t)

data Info = Info{
     state :: State
   , numPlayable :: Int
   , lMap :: [(Int,Int)] -- maps L index to axis
 }

-- Should be much faster at telling if something is checkmate.
fastLegalMoveSets :: State -> [MoveSet]
fastLegalMoveSets s =
  {-up (head.snd$buildHC s)-} uncurry (search []) (buildHC s)

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

search :: [[Int]] -> Info -> HCs AxisLoc -> [MoveSet]
search sncl inf@(Info s nP lmp) allhcs@(hc:hcs) =
  case mapM listToMaybe hc of
    Nothing -> search sncl inf hcs
    Just c ->
      let cell = zip [0..] c
          csmp = map fst c
          mvset = makeMoveset (map (snd.snd) cell)
          newState = apply mvset s
          xsecs = findProblems inf cell newState hc
          in
        case xsecs of
          [] -> if isKnownCheck newState Nothing || not (correctTurn newState) then error "uuh" else
            mvset : search (csmp:sncl) inf (sanecut inf hc
                  (XSec $ zipWith (\ (_,(a,_)) i -> (i,[a])) cell [0..]) ++ hcs)
          (xsec:_) ->  search (csmp:sncl) inf (sanecut inf hc xsec ++hcs)
search sncl inf [] = []

sanecut :: Info -> HC AxisLoc -> Sec -> [HC AxisLoc]
sanecut inf hc xsec = let
  a = hc `contain` [34, 11, 14, 45, 0, 0]
  r = remove hc xsec
  b = r `contains`[34, 11, 14, 45, 0, 0]
  res =  (r >>= sanity inf)
  c = res `contains` [34, 11, 14, 45, 0, 0]
  in {-if a/=c then error$ unlines(
    map show hc ++
    "":map (unlines. map show) r ++
    [ show xsec
    , show (a,b,c)
    ])  else-} res

-- Ensure that if a new timeline with l-index l1 must be created (i.e. it cannot be 'pass'),
--  then new timelines with l-indices less than l1 must be created
sanity :: Info -> HC AxisLoc -> [HC AxisLoc]
sanity (Info _ nP _) hc = if any null hc then [] else res
  where
    a = hc `contain` [46,0,25,0,0,0]
    (playFrom,new) = splitAt nP hc
    (bit,newres) = foldr (\ (n:ns) (b,rest) -> (b || not (isPass (snd n)), (if b && isPass(snd n) then ns else n:ns):rest)) (False,[]) new
    res = [playFrom++newres]
    b = res `contains` [46,0,25,0,0,0]
    res' = if a/=b then upi (putStrLn (unlines (map show new++[""]++map show newres++["",show bit])) >> return res)  else res


findProblems :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
--findProblems numPlayable cell hc s = concat [test numPlayable cell hc s | test <- [findChecks, testPresent, jumpsMatchLeaves, jumpOrderConsistent]]
findProblems inf cell s hc =
  jumpsMatchLeaves inf cell s hc ++
  jumpOrderConsistent inf cell s hc ++
  testPresent inf cell s hc ++
  map snd (findChecks inf cell s hc)

-- Check if a branch involves jumping to a source board of a branch that must be created later
-- Also check that there aren't any jumps to passes
jumpOrderConsistent :: Info -> Cub -> State -> HC AxisLoc -> [Sec]
jumpOrderConsistent (Info _ nP lmp) cell s hc = noJumpsToPass ++ sec
  where
    (playable,new) = splitAt nP cell
    (jumpSrcs,jumpDests) = unzip [(((l,t),ax),((l',t'),ax))
       | (ax,(ix,loc))<- new, Jump _ (l,t,_,_) (l',t',_,_) <- [loc]]
    conflicts = [ (lt,ax,ax')
                 | ((lt,ax),rest) <- zip jumpDests (tail$ tails jumpSrcs),
                     (lt',ax') <- rest, lt==lt' ]
    -- passes = [((ax,[ix]),t) | (ax,(ix,Pass (l,t))) <- playable]
    noJumpsToPass = [XSec [(ax,[ix]), filterAxis (\ (_,j) -> Just lt == (fstPair<$>jumpDest j)) ax' hc]
        | (ax,(ix,Pass lt)) <- playable, (lt',ax')<-jumpDests, lt==lt']
    sec = [XSec [filterAxis (\ (_,j) -> Just lt == (fstPair<$>jumpDest j)) ax hc,
                 filterAxis (\ (_,j) -> Just lt == (fstPair<$>jumpSource j)) ax' hc]
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

-- This will want optimising: there must be a better way to eliminate things that don't pass the present to the opponent
-- idea: put pass at the end of non-branching axes
--testPresent :: Info -> Cub -> State -> HC AxisLoc -> (Bool,[Sec])
testPresent (Info s@(nw,wtls,btls,col) nP _) cell newS hc = secs
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
    --newt = present newS
    activePasses = [((ax,[ix]),t) | (ax,(ix,Pass (l,t))) <- always , abs l<=nA+length tMovers]
    nonPasses = [t | (ax,(ix,loc)) <- always, (l,t) <- [getLTFromLoc loc] , abs l<=nA+length tMovers]
    mint = minimum [t | (ax,(ix,loc)) <- always, (l,t) <- [getLTFromLoc loc] , abs l<=nA+length tMovers]
    --minPass = minimum (map snd passes)
    isBad = any ((==mint).snd) activePasses && all (\(ax,(ix,Jump _ _ (_,dt,_,_))) -> dt>=mint) tMovers
    secs = {-if isBad==correctTurn newS then error$ unlines [
        "Either the results are being inspected when they shouldn't be, or something has gone wrong"
      , show cell
      ]
      else -}if isBad
        then [ XSec (sec:rs++[ filterAxis (\ (ix,Jump _ _ (_,dt,_,_))-> (dt>=t)) ax hc | (ax,(ix,loc))<-tMovers])
          | (sec,t) <- activePasses, t==mint]
        else []


findChecks :: Info -> Cub -> State -> HC AxisLoc -> [(String,Sec)]
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
        Just (pos',Full (col,target,_)) <- [foldr (\ x y -> y <|> Just x) Nothing ln]
        if isRoyal target && col /= playerCol
          then return (fromCells oldS ((pos,cl):ln) hc lmp)
          else []
      fixedmoves = do
        d <- fixed piece ++ fixedCapturing pieceCol piece
        case getAt newS (pos + d) of
          Just c@(Full (col,target,_)) ->
            if isRoyal target && col /= playerCol
              then return (fromCells oldS [(pos,cl),(pos+d,c)] hc lmp)
              else []
          _ -> []
  fixedmoves ++ dirmoves
  {-if Just (XSec [{-(4,[43,45,47,49,51,53,54,55,56,57,58]),(3,[6]),(0,[9]),(2,[25])-}])== listToMaybe (map snd mvs)
    then error$ unlines [
        show pos
      , show mvs
      , show (makeMoveset (map (snd.snd) cell))
      , show cell
      ]
    else mvs -}

-- Take the cross section in which the given cells have the given values
fromCells :: State -> [((Int, Int, Int, Int), Cell)] -> HC AxisLoc -> [(Int, Int)] -> (String,Sec)
fromCells s@(_,_,_,col) pcs hc lmp = (show (map fst pcs), XSec [
  filterAxis (\ (_, j) -> Just cell == (mBoard j >>= flip getAtBoard (x, y)) &&  nextT (snd (getLTFromLoc j)) col==t) ax hc
   | (pos@(l, t, x, y), cell) <- pcs,
     (flip nextT col <$> getTime s l)`elem` [Just t,Nothing],
     Just ax <- [lookup l lmp]] )


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
      bmts = [(l,[(m,getType pbs m) | m <- legalMovesFromBoard s pb++[[]] ])-- putting the pass at the end gives a speedup by a factor of around 500 on tests/silly.5dpgn
                | pb@(l,t) <- pbs]
      in makeAxes s bmts


-- Consider doing some sorting
makeAxes :: State -> [(Int,[(Move,MoveType)])] -> (Info, HCs AxisLoc)
makeAxes s lmvs =
  let newL = getNewL s
      axisParts = map (toAxes s newL) lmvs
      build (n,_) (ax,_) = zip [0..] (ax ++ [ loc | (_,laxs)<-axisParts, (m,loc)<-laxs, m==n])
      maxBranches = length$ filter (any isLeave . fst) axisParts
      newBranches = Pass (error "the l,t coordinates of branch passes shouldn't be inspected") : [ loc | (_,laxs)<-axisParts, (m,loc)<-laxs, m==newL]
      nP = length lmvs
      inf = Info s nP (zip (map fst lmvs ++ [newL,newL+signum newL .. newL+(signum newL)*(maxBranches-1)]) [0..])

    in (inf, [zipWith build lmvs axisParts ++ [zip [0..] newBranches | _ <- [1..maxBranches]]])

-- Turn a list of moves into an axis + some moves for other axes
toAxes :: State -> Int -> (Int,[(Move,MoveType)]) -> ([AxisLoc], [(Int,AxisLoc)])
toAxes s newL (oldL,mvs) =
  let axls = mvs >>= \ mty -> toLoc s mty oldL newL
  in first (nub.map snd) (partition (\x -> fst x==oldL) axls)

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
