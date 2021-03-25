Warning: this documentation is out of date because we're now using a sat solver,
so the remainder of the hypercuboid is represented by constraints in the SAT
solver, rather than a list of sub-hypercuboids

The goal of this module is to provide an efficient way to determine if a
position is checkmate. The way we go about this is to build an N-dimensional
hypercuboid containing all possible states after a moveset is made, then
eliminate illegal sections of the hypercuboid until we find a legal moveset.

> module Game.Chess.TimeTravel.FastCheckMate(fastLegalMoveSets) where

First we get some imports out of the way - general utilities and things specific
to this game.

> import Game.Chess.TimeTravel.SAT
> import Control.Arrow(first)
> import Control.Applicative((<|>))
> import Data.List(foldl', nub, partition, sortOn, tails)
> import Data.Maybe(listToMaybe, fromJust)
> import Game.Chess.TimeTravel.Utils
> import Game.Chess.TimeTravel.Moves
> import Game.Chess.TimeTravel.Printing (drawState,displayMoveSet)

Datatypes in particular are worth looking at - this is where the basic types
used throughout this application are described.

> import Game.Chess.TimeTravel.Datatypes

Haskell note: the type '[a]' means "List of things of type 'a'". Lists in
Haskell are immutable linked lists and are constructed from '[]' (the empty
list) and '(:)' (cons). '[1,2,3]' means '1:(2:(3:[]))'.

Outline
==========

'fastLegalMoveSets' is the only function we export - given a state, it returns
a list containing all legal movesets from that state. This list is empty if and
only if the result of the game is a checkmate or a draw. It is computed lazily,
so this can be used to detect checkmate without wasting time if there are
billions of possible moves.

We first build a structure containing all possibilities (and also compute some
info about the state that doesn't change across possibilities), then search that
structure for legal movesets.

> fastLegalMoveSets :: State -> [MoveSet]
> fastLegalMoveSets s = uncurry search (buildHC s)

Haskell note: 'uncurry' lets us pass 2 arguments at once. We could also define
'search :: (Info, HCs AxisLoc) -> [MoveSet]', but using 'uncurry' is more
idiomatic Haskell since it means that search can be partially applied.

To search a space, we take a point from the space, then look for reasons that
point might not be a legal moveset. If there aren't any points, we can stop.
If we don't find any reasons it's illegal, it must be a legal moveset, so it
should be in the returned list, and we need to continue searching the remainder
of the space. If we do find at least one reason it's illegal, then we can remove
all points that are illegal for the same reason, and continue searching the
remainder.

> search :: Info -> SearchSpace -> [MoveSet]
> search info space =
>   case takePoint info space of
>     (Nothing, _) -> []
>     (Just x, remainder) -> case findProblems info x of
>       [] -> makeMoveset x : search info (removePoint info x remainder)
>       (reason:_) -> search info (remove info reason remainder)

Note: There are many good reasons to remove points, most of which can sometimes
remove large numbers of points. One important thing here is that we only remove
pieces which we know contian a particular point ('x' in this case), so we don't
waste much time on optimizations that don't apply.

The Search Space
================

From a given state, there are a number (P) of boards that can be played on. We
could construct a P-dimensional hypercuboid where each axis corresponds to a
playable board, and moves from each board are numbered so that an integer point
in P-dimensional space within this hypercuboid identifies 1 move (or pass) from
each playable board.

This is a workable strategy; however, it ignores the order in which extra
timelines are created. This means that each point in the hypercuboid may
correspond to multiple movesets, each of which have to be tested for legality.
(TODO: this idea should be tried and profiled.)

Instead, we add more axes for each timeline that might be created. This gives us
an N-dimensional hypercuboid where N ≤ 2P. Each index on an axis corresponds to
a board that could be added to an axis or a pass (not adding any boards to that
axis). If there are multiple moves which cause a piece to leave a board (for
example (0T5)Qc3>>(0T4)c3 and (0T5)Qc3>>(0T3)c3 ) then these will correspond to
the same point on the axis for the timeline which they are leaving (0L), but to
different points on the axis where new boards are created (+1L).

As well as just storing the new board, we track some extra data to help with
things like building the moveset.

> data AxisLoc =
>     Phy Board Move -- full move data for a physical move (within one board)
>   | Arrive Board Coords Coords -- (origin of creating piece) (landing location (not counting jump))
>   | Leave Board Coords -- origin of leaving piece -- (could just use (x,y) but this is easier)
>   | Pass (Int,Int) -- l,t of passing board. These should not be inspected passes on newly created timelines, since the t coordinate is not well defined.
>    deriving (Eq)

Situations with more than 10 timelines can arise, each of which might have
around 50 available moves. Since you probably don't have on the order of 10^50
bits of memory available, we need a more efficient way of storing the
hypercuboid than storing that many points separately.

A hypercuboid is stored as a list of axes where each axis is a list of indexed
items. The length of the outer list is the dimension of the hypercuboid.

> type HC a = [[(Int,a)]]

While the indices for each axis will initially be '[0..n]', we will want to
consider subsets where they don't behave so nicely. Given a list of N indices
describing a point, we might ask whether a hypercuboid contains that point.

> contain :: HC a -> [Int] -> Bool
> contain [] [] = True
> contain (ax:axs) (ix:ixs) = ix `elem` (map fst ax) && contain axs ixs

When we cut pieces out of a hypercuboid, the remainder may not still be a
hypercuboid. To deal with this, we use a list of disjoint hypercuboids to
represent a union.

> type HCs a = [HC a]
> contains :: HCs a -> [Int] -> Bool
> contains hcs ixs = any (`contain` ixs) hcs

'takePoint' finds a point that has not yet been ruled out.

> takePoint :: Info -> SearchSpace -> (Maybe [(Int,AxisLoc)], SearchSpace)
> takePoint i ss = getPoint (zip [0..] (fullSpace i)) (numDims i) ss


Building the search space
=========================

To build the initial hypercuboid, we make use of some functions from
'Game.Chess.TimeTravel.Moves'. We begin by taking the playable boards, then
consider all moves from each such board. Each move then gives us a list of axis
locations (boards which are created by that move). We rule out moves that are
illegal by themselves here, although that could be done as part of the overall
search.

> buildHC :: State -> (Info, SearchSpace)
> buildHC s =
>   let pbs = playableBoards s
>       playableTimelines = map fst pbs
>       newL = getNewL s
>       allLocs = do -- List monad
>         (l,t) <- pbs
>         m <- legalMovesFromBoard s (l,t)
>         toLocs s l newL m (getType pbs m)

Next we split these up by axis location, remove duplicates within each axis
(these may arise when a piece can move to other boards in multiple ways), and
add the possibility of a pass (choosing not to move on a board).

>       locsOn l = nub [ loc | (l',loc) <- allLocs, l'==l]
>       nonBranchingAxes = [ locsOn l ++ [Pass (l,t)] | (l,t) <- pbs]

Note: some of the details here have large performance implications - putting the
pass at the end gives a speedup by a factor of around 500 on tests/silly.5dpgn

(TODO: profile the results of different orderings, in particular putting
physical (same-board) moves first)

New branches could appear on different timelines, so we make a copy of the
axis containing new branches for each timeline that could be created.

>       maxBranches = length$ filter (any isLeave) nonBranchingAxes
>       newBoards = Pass undefined : locsOn newL
>       branchingAxes = replicate maxBranches newBoards

The info consists of the state without any moves played, the number of playable
boards from that state (the axes corresponding to these timelines are the first
axes in the hypercube), and a map from L-indices of timelines to axes within the
hybercube given as a list of pairs of Ints.

>       nP = length playableTimelines
>       sign = signum newL
>       hc = map (zip [0..]) (nonBranchingAxes++branchingAxes)
>       info = Info s nP (zip (map fst pbs ++ [newL,newL+sign .. newL+sign*(maxBranches-1)]) [0..]) hc (length hc)
>       ss = addAssertion (All [Any [Sec (l,[x]) | (x,_) <- ax] | (l,ax) <- zip [0..] hc ]) (mkSpace hc)
>       branchingNumbered = drop (length nonBranchingAxes) (zip [0..] hc)
>       ss' = addAssertion (All [Not$All [Sec(l-1,[0]),Sec(l,map fst xs)] | (l,(_:xs)) <- drop 1 branchingNumbered]) ss
>     in (info, ss')
> data Info = Info{
>      state :: State
>    , numPlayable :: Int
>    , lMap :: [(Int,Int)] -- maps L index to axis
>    , fullSpace :: HC AxisLoc
>    , numDims :: Int
>  }


In 'toLocs', we split up jumping moves into a source and a destination. Hops
(moves between active boards) could also be branches, so we create an extra
location with the destination board associated with the L-index for new
timelines.

> toLocs :: State -> Int -> Int -> Move -> MoveType -> [(Int,AxisLoc)]
> toLocs s oldL newL m ty = -- I wish I had a reason to write f oldL
>   let newboards = foldl' (fullMove' s) [] m in
>     newboards >>= (\ ((l,t),b) ->
>       case ty of
>         SameBoard -> [(l,Phy b m)]
>         Hop landing -> if l==oldL
>           then [(l, Leave b (fst$head m))]
>           else let loc = uncurry (Arrive b) (head m)
>                  in [(landing,loc), (newL,loc)]
>         Branch -> if l==oldL && Just t==getTime s l
>           then [(l, Leave b (fst$head m))]
>           else [(newL, uncurry (Arrive b) (head m))]
>         _ -> error ("bad movetype"++show ty++"L:"++show oldL++";"++show l ))
>
> -- Optimization: consider cases where the present might move separately
> -- For determining checkmate, it is unnecessary to consider
> -- new TLs created later than the one which moves the present
> -- (note:if there are inactive timelines, don't forget about them being activated)

Removing pieces
===============

The most common type of piece we want to remove is a cross section. This is a
subhypercuboid defined by a list of axes and subsets of each axis in the list.
For axes not mentioned, the whole axis is implied.


For example, given the hypercuboid
hc = [[(0,()), (1,()), (2,())]
     ,[(10,()), (11,())]
     ,[(20,()), (21,()), (22,())]] :: HC ()
The cross section
xsec = [(0,[0,2]), (2,[21])]
indicates all points where the first coordinate is 0 or 2 and the third
coordinate is 21. When considered part of the hypercuboid hc, this means the
same as [(0,[0,2]), (1,[10,11]), (2,[21])]

To make a cross section contining a single point, we can do the following:

> xSecFromPoint :: [(Int,a)] -> Sec
> xSecFromPoint xs = Not$All (map Sec (zip [0..] [[x] | (x,_)<-xs]))


We also consider another type of piece which would be inefficient to implement
as a number of cross sections. This is used to make sure that if a piece
leaves a timeline, there is exactly one timeline where it arrives. This is
represented by the subset of the source axis which involves the piece leaving,
and subsets of each axis where the piece might arrive. 'remove'ing it means
cutting away all points unless they either don't match any of the subsets (i.e.
points that have nothing to do with the jump under consideration), or they match
the first axis, and exactly one of the others.


Note: Perhaps neater way of doing this would be to invert the first subset then
include it with the others. In this case, we would care about the subset which
match exactly one axis.

TODO: Consider using indicator functions rather than lists of coordinates (or
just use a more efficient data structure)


> remove :: Info -> Sec -> SearchSpace -> SearchSpace
> remove info sec ss = addAssertion sec ss

> removePoint :: Info -> [(Int,a)] -> SearchSpace -> SearchSpace
> removePoint info p = remove info (xSecFromPoint p)

When removing a piece from a hypercuboid, we apply some sanity checks to make
sure that if a new timeline with l-index l1 must be created (i.e. it cannot be
'pass'), then new timelines with l-indices less than l1 must be created.
This assumes that for axes corresponding to newly created timelines, 'Pass'es
will be at the start. It also gets rid of any empty hypercuboids (those where
some axis is empty)

> sanity :: Info -> HC AxisLoc -> [HC AxisLoc]
> sanity (Info _ nP _ _ _) hc = if any null hc then [] else res
>   where
>     (playFrom,new) = splitAt nP hc
>     (bit,newres) = foldr (\ (n:ns) (b,rest) -> (b || not (isPass (snd n)), (if b && isPass(snd n) then ns else n:ns):rest)) (False,[]) new
>     res = if any null newres then [] else [playFrom++newres]


Identifying illegal combinations
================================

Starting with a space that is just a hypercuboid has advantages, but means that
there are more reasons a cell might be illegal than a naive algorithm needs to
check for.

reasons a cell might be illegal:
- A king can be captured
- The present isn't moved
- It doesn't represent a consistent moveset:
  - there isn't an 'Arrive' matching a 'Leave'
  - the order of "branches that could be hops" is inconsistent
    - This includes problems involving cycles
  - an earlier (new) timeline might have a pass while a later one gets created

The last possibility was eliminated by the sanity check, so we don't need to
worry about it.
A lot of them could be dealt with by cutting up the hypercuboid before starting.
I don't believe that should be done because it will often be faster to rule out
very large pieces before splitting the hypercuboid.

We make sure that it represents a consistent moveset before testing for checks
and ensuring that the present is moved, or we would need to be more careful
about the assumtions made by the code doing those tests.

> findProblems :: Info -> [(Int,AxisLoc)] -> [Sec]
> findProblems info@(Info s _ _ _ _) point =
>   let cell = zip [0..] point
>       s' = apply (makeMoveset point) s
>     in arrivalsMatchLeaves info cell s' ++
>       jumpOrderConsistent info cell s' ++
>       testPresent info cell s' ++
>       findChecks info cell s'

Note: We rely on laziness here. Only the first problem we find gets examined,
so Haskell won't waste time computing the others (and it doesn't matter if they
would throw errors).
(TODO: make findChecks and testPresent more robust (or improve makeMoveset) so
that it's possible to compare all the problems and cut out bigger pieces)


For the various checks, it's useful to have axis numbers associated with each
coordinate of the cell, using the following shape:
(axis {-maps to L-},(index, move))

> type HCell = [(Int,(Int,AxisLoc))]

> filterAxis :: ((Int,a) -> Bool) -> Int -> HC a -> Sec
> filterAxis f ax axes = Sec (ax, map fst $ filter f (axes !! ax))


To confirm that there is a bijection between arrives and leaves, we test the
following:
- No two arrivals have the same source L-index
- If there is a board where a piece arrives from l, that piece must leave l
- If a piece leaves l, it must arrive on some board

> arrivalsMatchLeaves :: Info -> HCell -> State -> [Sec]
> arrivalsMatchLeaves (Info _ nP lmp hc i) cell s = noDups ++ srcMatches ++ mustAppear
>   where
>     arrivals = [(src,cl) | cl@(ax,(ix,loc))<- cell, Just src <- [arriveSource loc]]
>     sortedArrivals = sortOn fst arrivals
>     noDups = [ makeSec c |
>         ((c,(ax,_)),(c',(ax',_))) <- zip sortedArrivals (tail sortedArrivals), c==c'
>       ]
>     srcMatches = [ makeSec c |
>        (c@(l,_,_,_),(ax,_)) <- arrivals,
>        ax' <- [fromJust $ lookup l lmp], Just c /= (leaveSource.snd.snd) (cell!!ax')
>       ]
>     mustAppear = [ makeSec c |
>       (Just c)<- map (leaveSource.snd.snd) cell, c `notElem` map fst arrivals
>      ]
>     makeSec :: Coords -> Sec
>     makeSec c@(l,_,_,_) = let
>       Just ax = lookup l lmp in
>         ExactlyOne (Sec (ax,[ ix | (ix,loc)<-hc!!ax, leaveSource loc /= Just c]):
>           [Sec (ax', [ ix | (ix,loc) <-locs, arriveSource loc==Just c]) | (ax',locs) <- zip [0..] hc, ax'/=ax])

Give that there is a bijection between 'Arrive's and 'Leave's, we need to ensure
that there is some order in which the moves could be applied that results in the
new state. The order among moves that don't create new branches obviously
doesn't matter, and since we've chosen on which timelines the branches are
created, there's no question about the order of those moves either. We just need
to ensure that every move which creates a branch lands on a board which has been
played on. There are 2 ways this can go wrong:
- If the destination board is a pass.
- If the destination board is the source of a branch which was supposed to
  happen after this one.

> jumpOrderConsistent :: Info -> HCell -> State -> [Sec]
> jumpOrderConsistent (Info _ nP lmp hc i) cell s = noArrivesToPass ++ noLeavesAfterArrive
>   where
>     (playable,new) = splitAt nP cell
>     (jumpSrcs,jumpDests) = unzip [(((l,t),ax),((l',t'),ax))
>        | (ax,(ix,loc))<- new, Arrive _ (l,t,_,_) (l',t',_,_) <- [loc]]
>     conflicts = [ (lt,ax,ax')
>                  | ((lt,ax),rest) <- zip jumpDests (tail$ tails jumpSrcs),
>                      (lt',ax') <- rest, lt==lt' ]
>     noLeavesAfterArrive = [Not$All [filterAxis (\ (_,j) -> Just lt == (fstPair<$>arriveDest j)) ax hc,
>                  filterAxis (\ (_,j) -> Just lt == (fstPair<$>arriveSource j)) ax' hc]
>            | (lt,ax,ax') <- conflicts]
>     noArrivesToPass = [Not$All [Sec (ax,[ix]), filterAxis (\ (_,j) -> Just lt == (fstPair<$>arriveDest j)) ax' hc]
>         | (ax,(ix,Pass lt)) <- playable, (lt',ax')<-jumpDests, lt==lt']


This is almost exactly the same code from the naive search
('Game.Chess.TimeTravel.Moves'), it just tracks the boards involved in a check.

> findChecks :: Info -> HCell -> State -> [Sec]
> findChecks i@(Info oldS _ lmp hc _) cell newS@(_,_,_,playerCol) = do
>   (l,t) <- playableBoards newS
>   let Just b = getBoard newS (l,t)
>   (file,x) <- zip b [0..]
>   (cl@(Full (pieceCol,piece,_)),y) <- zip file [0..]
>   True <- return (pieceCol==playerCol)
>   let pos = (l,t,x,y)
>       dirmoves = do
>         d <- directions piece
>         let ln = line pos d (getAt newS)
>         Just (pos',Full (col,King,_)) <- [foldr (\ x y -> y <|> Just x) Nothing ln]
>         if col /= playerCol
>           then return (fromCells i ((pos,cl):ln) hc lmp)
>           else []
>       fixedmoves = do
>         d <- fixed piece ++ fixedCapturing pieceCol piece
>         case getAt newS (pos + d) of
>           Just c@(Full (col,King,_)) ->
>             if col /= playerCol
>               then return (fromCells i [(pos,cl),(pos+d,c)] hc lmp)
>               else []
>           _ -> []
>   fixedmoves ++ dirmoves

'fromCells' finds the cross section of a hypercuboid corresponding to movesets
after which the given cells have the given values.
(cells here means 4D `squares`, not N-dimensional hypercuboid cells)

> fromCells :: Info -> [((Int, Int, Int, Int), Cell)] -> HC AxisLoc -> [(Int, Int)] -> Sec
> fromCells (Info s@(_,_,_,col) _ _ _ i) pcs hc lmp = Not$All [
>   filterAxis (\ (_, j) -> Just cell == (mBoard j >>= flip getAtBoard (x, y))
>                           &&  nextT (snd (getLTFromLoc j)) col==t) ax hc
>    | (pos@(l, t, x, y), cell) <- pcs,
>      (flip nextT col <$> getTime s l)`elem` [Just t,Nothing],
>      Just ax <- [lookup l lmp]]

Ensuring that the present doesn't move has plenty of potential for bugs. I
should write a better explanation at some point.

We begin by finding which new timelines will be active when they are created,
('newActive') and then check whether there are any passes on active timelines
(including newly active ones) in the new present.

> testPresent :: Info -> HCell -> State -> [Sec]
> testPresent (Info s@(nw,wtls,btls,col) nP _ hc i) cell newS = secs
>   where
>     (playable,newBoards) = splitAt nP cell
>     nb = length wtls - nw - 1
>     (nMine,nOther) = if col==White then (nw,nb) else (nb,nw)
>     nPri = nOther + 1 - nMine -- how many active tls can be created?
>     (newActive,rest) = break (isPass.snd.snd) (take nPri newBoards)

If 'rest' is non-empty, there is a pass which could become an active timeline
and so move the present. If we find that the state is illegal, our reasoning
depends on this pass, so we must include it in the cross section.

>     rs = case rest of
>       [] -> []
>       ((ax,(ix,Pass _)):_) -> [Sec (ax,[ix])]
>     nA = numActive s
>     activePasses = [((ax,[ix]),t) | (ax,(ix,Pass (l,t))) <- playable , abs l<=nA+length newActive]
>     mint = minimum [t | (ax,(ix,loc)) <- playable, (l,t) <- [getLTFromLoc loc] , abs l<=nA+length newActive]
>     isBad = any ((==mint).snd) activePasses && all (\(ax,(ix,Arrive _ _ (_,dt,_,_))) -> dt>=mint) newActive
>     secs = if isBad
>         then [ (Not . All) (Sec sec:rs++[ filterAxis (isArrive.snd ^&&^ \ (ix,Arrive _ _ (_,dt,_,_))-> (dt>=t)) ax hc | (ax,(ix,loc))<-newActive])
>           | (sec,t) <- activePasses, t==mint]
>         else []

Note: I feel like there should be a better way to eliminate things that don't
pass the present to the opponent. Apart from checks which mostly care about
empty space, (and individual points when it finds a legal moveset) this is the
only place where we use more than 2 dimensions in a cross section, which means
it's an opportunity for inefficiency.


Helper functions for working with axis locations:

> instance Show AxisLoc where
>   show (Phy _ m) = "Phy"++show m
>   show (Arrive _ s t) = "Arrive"++show s++ show t
>   show (Leave _ m) = "Leave"++show m
>   show (Pass _) = "Pass"++show "unwilling to display"
>
> isLeave (Leave _ _) = True
> isLeave _ = False
> isArrive Arrive {} = True
> isArrive _ = False
> isPass (Pass _) = True
> isPass _ = False
>
> arriveSource :: AxisLoc -> Maybe Coords
> arriveSource (Arrive _ src _) = Just src
> arriveSource _ = Nothing
> arriveDest :: AxisLoc -> Maybe Coords
> arriveDest (Arrive _ _ dest) = Just dest
> arriveDest _ = Nothing
>
> leaveSource :: AxisLoc -> Maybe Coords
> leaveSource (Leave _ c)= Just c
> leaveSource _ = Nothing
>
> mBoard :: AxisLoc -> Maybe Board
> mBoard (Phy b _) = Just b
> mBoard (Arrive b _ _) = Just b
> mBoard (Leave b _) = Just b
> mBoard (Pass _) = Nothing
>
> getLTFromLoc :: AxisLoc -> (Int, Int)
> getLTFromLoc (Pass lt) = lt
> getLTFromLoc (Phy b (((l,t,_,_),_):_)) = (l,t)
> getLTFromLoc (Arrive b _ (l,t,_,_)) = (l,t)
> getLTFromLoc (Leave _ (l,t,_,_)) = (l,t)

> makeMoveset :: [(Int,AxisLoc)] -> MoveSet
> makeMoveset = (>>= toMove) . map snd -- branches are already at the end
>   where
>     toMove :: AxisLoc -> [Move]
>     toMove (Pass _) = []
>     toMove (Leave _ _) = []
>     toMove (Phy _ mv) = [mv]
>     toMove (Arrive _ src dest ) = [[(src,dest)]]
