{-# LANGUAGE BlockArguments #-}

module Game.Chess.TimeTravel.Moves
where
import qualified Data.Map.Lazy as M
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Utils

-- Debugging
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Layouts



legalMoveSets :: State -> [MoveSet]
legalMoveSets s = do -- list monad
  -- get possible moves from the first playable board (including "pass/arrive")
  let pbs = playableBoards s
  let bmts = [(l,[(m,getType pbs m) | m <- []:legalMovesInBoard s pb ]) | pb@(l,t) <- pbs]
  unorderedmoveset <- findLegal1 [] [] bmts
  True <- return (mightMoveTime unorderedmoveset s)
  perm <- validPermutations unorderedmoveset
  let s' = apply perm s
  False <- return$ isKnownCheck s' Nothing
  True <- return$ correctTurn s'
  return perm

correctTurn :: State -> Bool
correctTurn s@(nw,wtls,btls,col) =
  let nb = length wtls - nw - 1
      nactive = (min nb nw) + 1
      (wtls',btls') = unzip $ take (nw + 1 + nactive) $ drop (nw - nactive) (zip wtls btls)
      wt = minimum (map fst wtls')
      bt = minimum (map fst btls')
      in (bt<wt)==(col==White)


-- We need to search for all topological sorts of branching moves  given hops
-- (we must move from a board before jumping to it)
-- As there is at most 1 branching move from each timeline, the structure is a forest.
data Tree a = Node a (Forest a)
type Forest a = [Tree a]

type UnorderedMoveset = [(Int,Move,MoveType)]


-- Either all earliest active boards are played on or at least one move goes back in time and there is an active timeline available
-- TODO: actually implement this
mightMoveTime :: [(Int, Move, MoveType)] -> State -> Bool
mightMoveTime _ _ = True

validPermutations :: [(Int,Move,MoveType)] -> [MoveSet]
validPermutations mvs = do
  let (sameboardmoves,rest) = partition (\ (_,_,typ) -> typ==SameBoard) mvs
      (branches,rest') = partition (\ (_,_,typ) -> typ==Branch) rest
      (passes,hops) = partition (\ (_,_,typ) -> typ==NoAct) rest'
  let moveinfo = M.fromListWith (++) [(l,[nmt]) | nmt@(_,_,Hop l) <- hops]
  -- build a forest from the moveinfo
  let branchTrees = (foldr (++)
        -- each branch can appear in any order
        [Node m (makeForest l moveinfo) | (l,m,_) <- branches]
        -- if there's a move within a board, all hops to that board must be branches
        [makeForest l moveinfo | (l,_,_) <- sameboardmoves])
  --passes which have multiple hops to them are trickier
  -- for each such pass, we take each hop that could be a move between playable
  -- boards and consider the possibility that it is the
  (betweenBoardmoves, newbranches) <- map unzip $ maybeProd [ removeEach <$> M.lookup l moveinfo | (l,n,t) <- passes]
  let branchTrees' = [Node m (makeForest l moveinfo) | (l,m,_) <- join newbranches]++branchTrees

  fromForest [m | (_,m,_) <- sameboardmoves++betweenBoardmoves] branchTrees'

-- Build a forest representation of the directed graph of hops given by moveinfo, starting at n
makeForest :: Int -> M.Map Int [(Int,Move,MoveType)] -> Forest Move
makeForest n moveinfo = [ Node m (makeForest l moveinfo) | (l, m,_) <- M.findWithDefault [] n moveinfo]

--
fromForest :: MoveSet -> Forest Move -> [MoveSet]
fromForest prefix [] = [reverse prefix]
fromForest prefix ne = do
  (Node m bs,xs) <- removeEach ne
  fromForest (m:prefix) (bs++xs)


-- removeEach [x] = [(x,[])]
removeEach :: [a] -> [(a,[a])]
removeEach [] = []
removeEach (x:xs) = (x,xs):map (second (x:)) (removeEach xs)

-- take the product of a set of moves while avoiding cycles of hops (TODO: while testing for simple checks)
findLegal1 :: [Int] -> [(Int,Move,MoveType)] -> [(Int,[(Move,MoveType)])] ->  [[(Int,Move,MoveType)]]
findLegal1 nojumps acc ((l, mts ): bmts ) = do
  (move, movetype) <- mts
  case movetype of
      Hop n -> if n `elem` nojumps then [] else
        case span (\ (ll,_) -> ll/=n) bmts of
          (as,[]) -> findLegal1 [] ((l,move,movetype):acc) bmts
          (as,(b:bs)) -> findLegal1 (l:nojumps) ((l,move,movetype):acc) (b:(as++bs))
      m -> findLegal1 [] ((l,move,movetype):acc) bmts

findLegal1 [] acc [] = [acc]
findLegal1 nojumps acc [] = error "If there's an incomplete cycle, there should be unprocessed boards"



-- NoAct means either do nothing because the present is behind/moving backwards or that a piece is moved to this board by a hop
-- Hop means either a hop or a branch to a board that must be played on as part of this moveset
data MoveType = NoAct | SameBoard | Hop Int | Branch deriving (Eq)

getType :: [(Int,Int)] -> Move -> MoveType
getType _ [] = NoAct
getType pbs (((l1,t1,_,_),(l2,t2,_,_)):mvs) = if (l1,t1)==(l2,t2) then SameBoard
  else if (l2,t2) `elem` pbs then Hop l2 else Branch

passMove :: (Int,Int) -> Move
passMove (l,t) = []--[((l,t,0,0),(l,t,0,0))]


legalMovesInBoard :: State -> (Int,Int) -> [Move]
legalMovesInBoard s@(_,_,_,playerCol) (l,t) = do
  m <- movesInBoard s (l,t)
  let t' = nextT t playerCol
  if isKnownCheck (halfApply m s) (Just (l,t')) then [] else return m

-- Apply a moveset to a
apply :: MoveSet -> State -> State
apply mvs s = flipPlayer$ foldl' fullMove s mvs

fullMove :: State -> Move -> State
fullMove s m = foldl' addBoardFull s (foldl' (fullMove' s) [] m)

-- Add a board, making a new timeline if needed
addBoardFull :: State -> ((Int,Int),Board) -> State
addBoardFull s@(n,wtls,btls,col) ((l,t),b) = let t' = nextT t col in case getBoard (flipPlayer s) (l,t') of
  Just _ -> case col of
    White -> (n+1, (t',[]):wtls, (t',[b]):btls, col)
    Black -> (n+1, (t',[b]):wtls, (t ,[]):btls, col)
  Nothing -> addBoard s l b

nextT :: Int -> Player -> Int
nextT n White = n
nextT n Black = n+1

fullMove' :: State -> [((Int,Int),Board)] -> (Coords,Coords) -> [((Int,Int),Board)]
fullMove' s modifiedBoards ((l,t,x,y),(l',t',x',y')) =
  let b = fromMaybe (fromJust (getBoard s (l,t))) (lookup (l,t) modifiedBoards)
      (p,b') = removePiece (x,y) b
      modifiedBoards' = update (l,t) b' modifiedBoards
      b'' = fromMaybe (fromJust (getBoard s (l',t'))) (lookup (l',t') modifiedBoards')
      in update (l',t') (placePiece (setMoved p) (x',y') b'') modifiedBoards'


update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update x y xys =  (x,y) : filter ((/=x).fst) xys

-- NOTE: relies on the assumption that fpr moves with multiple parts, all parts
-- originate from the same board
halfApply :: Move -> State -> State
halfApply mvs@(((l,t,_,_),_):_) s = flipPlayer (addBoard s l (foldl' halfMove (fromJust$ getBoard s (l,t)) mvs))

flipPlayer :: State -> State
flipPlayer (n,wtls,btls,p) = (n,wtls,btls,other p)

halfMove :: Board -> (Coords, Coords) -> Board
halfMove b ((l,t,x,y),(l',t',x',y')) = let (p,b') = removePiece (x,y) b in
  if (l',t')==(l,t) then placePiece (setMoved p) (x',y') b' else b'

addBoard :: State -> Int -> Board -> State
addBoard (n,wtls,btls,White) l b = (n,wtls,modifyAt btls (n-l) ((+1)*** (b:)), White)
addBoard (n,wtls,btls,Black) l b = (n,modifyAt wtls (n-l) ((+1)*** (b:)), btls, Black)

removePiece :: (Int,Int) -> Board -> (Cell,Board)
removePiece (0,y) (f:board) = second (:board) (removePieceFile y f )
removePiece (n,y) (f:board) = second (f:) (removePiece (n-1,y) board)

removePieceFile :: Int -> [Cell] -> (Cell,[Cell])
removePieceFile 0 (c:f) = (c,Empty:f)
removePieceFile n (c:f) = second (c:) (removePieceFile (n-1) f)

placePiece :: Cell -> (Int,Int) -> Board -> Board
placePiece p (0,y) (f:b) = placePieceFile p y f : b
placePiece p (x,y) (f:b) = f: placePiece p (x-1,y) b

placePieceFile :: Cell -> Int -> [Cell] -> [Cell]
placePieceFile p 0 (c:f) = p:f
placePieceFile p y (c:f) = c: placePieceFile p (y-1) f

setMoved :: Cell -> Cell
setMoved (Full (c,p,Still)) = Full (c,p,Moved)
setMoved x = x

movesInBoard :: State -> (Int,Int) -> [Move]
movesInBoard s@(_,_,_,playerCol) (l,t) = let Just b = getBoard s (l,t) in
  do
    (file,x) <- zip b [0..]
    (Full (pieceCol,piece,_),y) <- zip file [0..]
    True <- return (pieceCol==playerCol)
    let pos = (l,t,x,y)
        dirmoves = do
          d <- directions piece
          ( pos',cell) <- line pos d (getAt s)
          case cell of
            Full (col,_,_) -> if col /= playerCol then [[(pos,pos')]] else []
            Empty -> [[(pos,pos')]]
        fixedmoves = do
          d <- fixed piece
          case getAt s (pos + d) of
            Just (Full (col,_,_)) -> if col /= playerCol then [[(pos,pos + d)]] else []
            Just Empty -> [[(pos,pos + d)]]
            Nothing -> []
    dirmoves ++ fixedmoves ++ special pos s


-- Checks if opposing pieces moving from the given board could capture any king
isKnownCheck :: State -> Maybe (Int,Int) -> Bool
isKnownCheck s@(_,_,_,playerCol) lt = not$ null do
  (l,t) <- case lt of
    Just lt' -> [lt']
    Nothing -> playableBoards s
  --let Just b = getBoard (upi (putStrLn (drawState standard s) >> print lt >> return s)) (l,t)
  let Just b = getBoard s (l,t)
  (file,x) <- zip b [0..]
  (Full (pieceCol,piece,_),y) <- zip file [0..]
  True <- return (pieceCol==playerCol)
  let pos = (l,t,x,y)
      dirmoves = do
        d <- directions piece
        ( pos',cell) <- line pos d (getAt s)
        case cell of
          Full (col,King,_) -> if col /= playerCol then [()] else []
          _ -> []
      fixedmoves = do
        d <- fixed piece ++ fixedCapturing pieceCol piece
        case getAt s (pos + d) of
          Just (Full (col,King,_)) -> if col /= playerCol then [()] else []
          _ -> []
  fixedmoves ++ dirmoves

-- note: tw>tb iff the last board in a timeline is white (created by black, white to play)
playableBoards :: State -> [(Int,Int)]
playableBoards (n,wtls,btls,col) =
  join$ zipWith3
    (\ (tw,_) (tb,_) l -> if (tw==tb)/=(col==White) then [(l,tw)] else [] )
     wtls btls [n,n-1..]


-- King | Knight | Bishop | Rook | Queen | Pawn | Unicorn | Dragon
unitl = (1,0,0,0)
unitt = (0,1,0,0)
unitx = (0,0,1,0)
unity = (0,0,0,1)
units = [(1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)]

box :: [[Int]]
box = replicateM 4 [-1..1]

toCoords :: [Int] -> Coords
toCoords [a,b,c,d] = (a,b,c,d)

qm = map toCoords$ filter (any (/= 0)) box
dm = map toCoords$ filter (all (/=0)) box
um = map toCoords$ filter ((==3) . length . (filter (/=0))) box
bm = map toCoords$ filter ((==2) . length . (filter (/=0))) box
rm = map toCoords$ filter ((==1) . length . (filter (/=0))) box
nm =  [ da*(units!!a) + db*(units!!b)  | a <- [0..3], b <- [a+1 .. 3], dx <- [-1,1], dy <-[-2,2], (da,db) <- [(dx,dy),(dy,dx)]]

wpncm = [unity, unitx]
bpncm = map negate wpncm
wpcm =  [ -unitl+unitt, -unitl-unitt, unity+unitx, unity-unitx]
bpcm = map negate wpcm

-- Moves that can continue as far as possible in one direction
directions :: Piece -> [Vector]
directions King = []
directions Knight = []
directions Pawn = []
directions Rook = rm --units ++ map (**** (-1)) units
directions Bishop = bm -- [ x****(units!!a) ++++ y****(units!!b) | (a,b) <- planes, x <- [1,-1], y <- [1,-1]]
directions Queen = qm
directions Unicorn = um
directions Dragon = dm

line :: Num a => a -> a -> (a -> Maybe Cell) -> [(a,Cell)]
line start delta get = let n = start+delta in
  case get n of
    Nothing -> []
    Just Empty -> (n,Empty):line n delta get
    Just p@(Full _) -> [(n,p)]

-- moves that cannot continue indefinitely
fixed :: Piece -> [Vector]
fixed King = qm
fixed Knight = nm
fixed _ = []

-- fixed moves only allowed when capturing
fixedCapturing :: Player -> Piece -> [Vector]
fixedCapturing White Pawn = wpcm
fixedCapturing Black Pawn = bpcm
fixedCapturing _ _ = []

pawnDirections :: Player -> [Vector]
pawnDirections White = [-unitl, unity]
pawnDirections Black = [unitl, -unity]


-- other moves that depend on the state of the game and may affect more than 2 squares
-- en passant, castle, pawn double move
special :: Coords -> State -> [Move]
special pos@(l,t,x,y) state@(_,_,_,playerCol) =
  let Just b = getBoard state (l,t) in
    case (getAtBoard b (x,y)) of
      -- could test that pieceCol == playerCol
      Just (Full (pieceCol,King,Still)) -> map (map (((l,t)|+) *** ((l,t)|+)))
          (castles (x,y) b pieceCol)
      Just (Full (pieceCol,Pawn,moved)) -> pawnmoves pos state moved
      Just (Full _) -> []
      _ -> error "no piece at given position"

type BoardMove = ((Int,Int),(Int,Int))

-- Assuming an unmoved king is at 'pos',
castles :: (Int,Int) -> Board -> Player -> [[BoardMove]]
castles pos board playerCol = [(1,0),(-1,0)] >>= \ km -> castle km pos board playerCol


castle :: (Int,Int) -> (Int,Int) -> Board -> Player -> [[BoardMove]]
castle dxy pos board playerCol =
  case firstrook (pos,dxy) board playerCol of
    Just p | p/=(pos+dxy) && not (any (\ p -> checks p board playerCol) [pos,pos+dxy,pos+2*dxy])
               -> [[(p,pos+dxy),(pos,pos + 2*dxy)]]
    _ -> []

-- returns the coordinates of any unblocked, unmoved rook in the given direction
-- technically, we don't have to worry about colour, because that will be caught by check detection
firstrook :: CastleData -> Board -> Player -> Maybe (Int,Int)
firstrook (pos,dxy) board playerCol = foldr f Nothing (line pos dxy (getAtBoard board))
  where
    f x y = case x of
      (pos', Full (pieceCol,Rook,Still)) | pieceCol==playerCol -> Just pos'
      _ -> y -- 'line' ends at first piece

-- is a square threatened within a single board (for castling)?
-- position, board, colour of the player trying to castle
checks :: (Int,Int) -> Board -> Player -> Bool
checks pos b col =
  or do
    (x, file) <- zip [0..] b
    (y, Full (piececol,piece,_)) <- zip [0..] file
    _ <- if piececol==col then [] else [()]
    let pos'=(x,y)
    return (or [ checkdir pos pos' (dx,dy) (getAtBoard b) |
              (0,0,dx,dy) <- directions piece]
         || or [ pos'+(dx,dy)==pos |
              (0,0,dx,dy) <- fixedCapturing piececol piece ++ fixed piece])

-- is target hit by the line from start in direction delta
checkdir :: (Num a, Eq a) => a -> a -> a -> (a -> Maybe Cell) -> Bool
checkdir target start delta get = target `elem` map fst (line start delta get)


pawnmoves :: Coords -> State -> Moved -> [Move]
pawnmoves pos state@(_,_,_,playerCol) moved =
  -- single moves, double moves, en passant
  (do -- list monad, en passant
    d@(0,0,dx,dy) <- fixedCapturing playerCol Pawn
    let sq = pos+d
    Just Empty <- [getAt state sq] -- this check is not be needed (unless more complicated moves get added)
    let vdy = (0,0,0,dy)
    [Just (Full (_,Pawn,Still)), Just Empty,
     Just Empty, Just (Full (_,Pawn,Moved))] <- return$
      map (getAt state . (sq +))  [vdy-unitt,-vdy-unitt,vdy,-vdy]
    return [(pos,sq),(pos,sq-vdy)]
    )
  ++ do -- list monad, non-capturing
    d <- pawnDirections playerCol
    Just Empty <- [getAt state (pos+d)]
    if moved == Still && getAt state (pos+2*d) == Just Empty then
      [[(pos,pos+d)],[(pos,pos+2*d)]] else [[(pos,pos+d)]]
