import Game.Chess.TimeTravel.Parser
import Game.Chess.TimeTravel.PGNParser
import Game.Chess.TimeTravel.Printing
import Game.Chess.TimeTravel.Layouts(turnZero)
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves
import Game.Chess.TimeTravel.BuildGame
import Game.Chess.TimeTravel.Utils
import Game.Chess.TimeTravel.FastCheckmate

-- import Data.Array.Unboxed
--import Data.Array.IO

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign
import Data.Word
import Data.Int
import Data.Char
import Data.IORef
import Data.List(foldl')
import Control.Applicative(liftA2)

import System.IO.Unsafe(unsafePerformIO)

(^+^) = liftA2 (+)
foreign export ccall fib :: Int -> IO Int
fib 0 = return 0
fib 1 = return 1
fib n = fib (n-1) ^+^ fib (n-2)

maxT :: Int
maxT = 128
maxL :: Int
maxL = 64
minL :: Int
minL = -maxL
numTimelines :: Int
numTimelines = (maxL - minL) + 1

moveSize :: Int
moveSize = 8

-- Outputs

-- output (board, bytes)
numCells :: Int
numCells = numTimelines*maxT*64*2

-- output ( minL, maxL 1 byte each; timeline starts and ends, 2 bytes per timeline)
boardShapeDataSize :: Int
boardShapeDataSize =  2{-minL,maxL-} + numTimelines*2 -- values

--output
checkDataSize :: Int
checkDataSize = moveSize

-- Inputs
-- (also output indicating moves from a board)
-- input (moves, 8 bytes each)
maxMoves :: Int
maxMoves = numTimelines*(maxT-1)
-- input (bytes to store the number of movesnumber of Moves)
--numMovesSize :: Int
--numMovesSize = 2
-- max moves by a queen

maxOutMoves :: Int
maxOutMoves = 7*4*(1+8) + numTimelines + maxT
outMoveSize :: Int
outMoveSize = 4


foreign export ccall initMem :: IO (Ptr Word8)
initMem = do
    ptr <- mallocArray (maxMoves*moveSize + maxOutMoves*outMoveSize + checkDataSize + boardShapeDataSize + numCells)
    s <- readIORef globalState
    ptr' <- writeState s ptr
    return ptr'

foreign export ccall reset :: Ptr Word8 -> IO (Ptr Word8)
reset ptr = do
    writeIORef globalState turnZero
    writeState turnZero ptr

inputMovesDataStart = 0 :: Int --inputMovesStart + numMovesSize :: Int
outputMovesDataStart = inputMovesDataStart + maxMoves*moveSize :: Int
checkDataStart = outputMovesDataStart + maxOutMoves*outMoveSize
boardShapeStart = checkDataStart + checkDataSize
foreign export ccall boardDataStart :: Int
boardDataStart = boardShapeStart + boardShapeDataSize

co :: (Num a,Integral b) => b -> a
co = fromInteger . toInteger

toW8 :: (Integral b) => b -> Word8
toW8 = co
frW8 :: (Ptr Word8)  -> IO Int
frW8 ptr = co <$> (peek ptr :: IO Word8)
frI8 :: (Ptr Word8) -> IO Int
frI8 ptr = co <$> (peekByteOff ptr 0 :: IO Int8)

toInt :: (Integral b) => b -> Int
toInt = co

writeBoard :: Board -> Ptr Word8 -> IO ()
writeBoard xss ptr = mapM_ (\ (fn, file)->
    mapM_ (\ (rn, cell) ->
        pokeByteOff ptr (fn*8+rn) (toW8 $ ord $ showc cell)
        ) (zip [0..]  file)
    ) (zip [0..] xss)

writeState :: State -> Ptr Word8 -> IO (Ptr Word8)
writeState s@(n,wtls,btls,p) ptr = do
    pokeByteOff ptr (boardShapeStart) (toW8$ n - length wtls + 1)
    pokeByteOff ptr (boardShapeStart+1) (toW8 n)
    mapM_ (\ (l, ((nw,wbs) ,(nb,bbs))) -> do
        let tStartEndOff = boardShapeStart+2 + (l - minL)*2
        let end = max (nw*2) (nb*2+1)
        let start = min ((nw - length wbs + 1)*2) ((nb - length bbs + 1)*2 +1)
        pokeByteOff ptr tStartEndOff (toW8 start)
        pokeByteOff ptr (tStartEndOff+1) (toW8 end)
        let boardsoff = boardDataStart + (l - minL)*maxT*64*2
        mapM_ (\ (t,brd) -> writeBoard brd (ptr `plusPtr` (boardsoff + t*2*64))
            ) (zip [nw,nw-1 ..] wbs)
        mapM_ (\ (t,brd) ->writeBoard brd (ptr `plusPtr` (boardsoff + (t*2+1)*64))
            ) (zip [nb,nb-1 ..] bbs)
        ) (zip [n,n-1 ..] (zip wtls btls))
    let s' = flipPlayer s
    writeCoords (ptr `plusPtr` checkDataStart) $
        case getKnownCheck s' Nothing of
         Just (src,dst) -> [src,dst]
         Nothing -> if (correctTurn s') then [0,0] else [-1,-1]
    return ptr

foreign export ccall bar :: (Ptr Word8) -> IO Int
bar p = do
    writeState turnZero p
    return 9


{-# INLINE liftA4 #-}
liftA4 :: Applicative f =>
   (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
mapA4 :: Applicative m => (a->m b)-> (a,a,a,a)->m (b,b,b,b)
mapA4 f (a,b,c,d) = liftA4 (,,,) (f a) (f b) (f c) (f d)

readPos :: Ptr Word8 -> IO Coords
readPos ptr = mapA4 (\(f,n) -> f (ptr `plusPtr` n)) ((frI8,0), (frW8,1), (frW8,2), (frW8,3))
readMove :: Ptr Word8 -> IO (Coords,Coords)
readMove ptr = liftA2 (,) (readPos ptr) (readPos (ptr `plusPtr` 4))

applyMvs :: State -> [(Coords,Coords)] -> State
applyMvs s = foldl' (fullMove) s  . map (getMoveDetails s)
getMoveDetails :: State -> (Coords,Coords) -> Move
getMoveDetails s mv@(src,dst) =
    let [move] = filter (\ parts-> any (==mv) parts) (movesFromSquare s src)
    in move

getStateWithMoves :: Ptr Word8 -> Int -> IO State
getStateWithMoves ptr nmvs = do
    mvs <- mapM (\ i -> readMove (ptr `plusPtr` (inputMovesDataStart+i*8) )) [0,1.. nmvs-1]
    s <- readIORef globalState
    let s' = applyMvs s mvs
    --TODO: test moveset legality ((a) not in check (b) moves present?)
    return s'

foreign export ccall changeMoves :: Ptr Word8 -> Int -> IO (Ptr Word8)
changeMoves ptr nMoves = do
    s <- getStateWithMoves ptr nMoves
    writeState s ptr
    
foreign export ccall submit :: Ptr Word8 -> Int -> IO (Ptr Word8)
submit ptr nMoves = do
    s <- flipPlayer <$> getStateWithMoves ptr nMoves
    writeIORef globalState s
    writeState s ptr
    --return ptr

writeCoords :: Ptr Word8 -> [Coords] -> IO ()
writeCoords ptr dests =
    mapM_ (\ (i,(l,t,x,y)) -> do
        mapM_ (\ (j,v) -> 
            pokeByteOff ptr (i*outMoveSize + j) (toW8 v)
            ) (zip [0..] [l,t,x,y])
        ) (zip [0..] dests)

foreign export ccall getMovesFromSquare :: Ptr Word8 -> Int -> Int -> Int -> Int -> IO Int
getMovesFromSquare ptr l t x y = do
    let pos = (l,t,x,y)
    s <- readIORef globalState
    let mvs = movesFromSquare s pos
    let dests = [dst | ((_,dst):_)<- map (filter ((==pos).fst)) mvs]
                --map (snd .head .filter ((==pos).fst)) mvs
    writeCoords (ptr`plusPtr`outputMovesDataStart) dests
    return$ length dests

-- globalState is bad :P , but we need to efficiently preserve data between calls into Haskell
globalState :: IORef State
globalState = unsafePerformIO (newIORef turnZero)

dat ::[[(Coords,Coords)]]
dat = [[((0,1,6,0),(0,1,5,2))],[((0,1,6,7),(0,1,5,5))],[((0,2,5,2),(0,2,3,3))],[((0,2,5,5),(0,2,3,4))],[((0,3,3,3),(0,3,5,2))],[((0,3,3,4),(0,3,5,5))],[((0,4,5,2),(0,4,3,3))],[((0,4,5,5),(0,4,3,4))],[((0,5,3,3),(0,5,5,2))],[((0,5,3,4),(0,5,5,5))],[((0,6,5,2),(0,6,3,3))],[((0,6,5,5),(0,6,3,4))],[((0,7,3,3),(0,7,5,2))],[((0,7,3,4),(0,7,5,5))],[((0,8,5,2),(0,8,3,3))],[((0,8,5,5),(0,8,3,4))],[((0,9,3,3),(0,9,5,2))],[((0,9,3,4),(0,9,5,5))],[((0,10,5,2),(0,10,3,3))],[((0,10,5,5),(0,10,3,4))],[((0,11,3,3),(0,11,5,2))]]

foreign export ccall baz :: (Ptr Word8) -> IO Int
baz ptr = do
    ptr' <- reset ptr
    mapM_ (\ms -> do
        let cs = ms >>= (\(a,b)->[a,b])
        writeCoords (ptr `plusPtr` inputMovesDataStart) cs
        changeMoves ptr (length ms)
        submit ptr (length ms)
      ) dat
    return 0
    --nmvs <- toInt <$> (peekByteOff ptr (inputMovesStart) :: IO Word16)
    --mvs@(((l,t,x,y),_):_) <- mapM (\ i -> readMove (ptr `plusPtr` (inputMovesDataStart+i*8) )) [0,1.. nmvs-1]
    -- s@(_,_,_,pl) <- readIORef globalState
    -- return (if pl==Black then 77 else 88)

main :: IO ()
main = mempty
