{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Game.Chess.TimeTravel.Datatypes where

import Game.Chess.TimeTravel.Utils

data Piece = King | Knight | Bishop | Rook | Queen | Pawn | Unicorn | Dragon deriving (Eq)

instance Show Piece where
  show King = "K"
  show Knight = "N"
  show Bishop = "B"
  show Rook = "R"
  show Queen = "Q"
  show Pawn = "P"
  show Unicorn = "U"
  show Dragon = "D"

data Player = White | Black deriving (Eq)

instance Show Player where
  show White = "w"
  show Black = "b"

other White = Black
other Black = White

data Moved = Moved | Still deriving (Eq) -- indicates if the piece has ever moved - for pawns and castling

type ColouredPiece = (Player,Piece,Moved)


data Cell = Empty | Full ColouredPiece deriving (Eq)
--  type Cell = Maybe ColouredPiece


-- list of files ('a' file at head, rank 1 at head of each file)
type Board = [[Cell]]


-- Turn of last board, boards (most recent at head)
-- only includes boards of one colour
type Timeline = (Int,[Board])


-- (L-index of last TL (# of TLs created by white), timelines with white boards (with those most recently created by white at the head) , timelines with black boards, Turn)
type State = (Int,[Timeline],[Timeline],Player)

-- L, T, x(file), y(rank)
type Coords = (Int,Int,Int,Int)
-- Note that L increases going down the screen whereas y increases going up the screen

getBoard :: State -> (Int,Int) -> Maybe Board
getBoard (n,wtls,_,White) (l,t) = wtls !!? (n - l) >>= flip getBoardTL t
getBoard (n,_,btls,Black) (l,t) = btls !!? (n - l) >>= flip getBoardTL t

getBoardTL :: Timeline -> Int -> Maybe Board
getBoardTL (n,bs) t  = bs !!? (n - t)

getAt :: State -> Coords -> Maybe Cell
getAt s (l,t,x,y) = getBoard s (l,t) >>= flip getAtBoard (x,y)

--getAtTL :: (Int,Int,Int) -> Timeline -> Maybe Cell
--getAtTL (t,x,y) (n,bs)  = bs !!? (n - t) >>= getAtBoard (x,y)

getAtBoard :: Board -> (Int,Int) -> Maybe Cell
getAtBoard css (x,y) = css !!? x >>= (!!? y)

type Vector = (Int,Int,Int,Int)

-- [source, destination] (needs to be a list for castling and en-passant
type Move = [(Coords,Coords)]

-- order matters
type MoveSet = [Move]

type CastleData = ((Int,Int),(Int,Int))

-- Initial boards, castle manouvers (a castle is represented as a king position (within a 2d board) and a direction.
-- The king may castle with any rook in that direction (provided that the intervening spaces are empty and neither piece has moved))
type Game = ([Board], [CastleData])



--infix 7  ****
--infixl 6  ++++
infix 5 |+
(a,b)|+(c,d) = (a,b,c,d)



instance Num (Int,Int,Int,Int) where
  (a,b,c,d) + (w,x,y,z) = (a+w,b+x,c+y,d+z)
  (a,b,c,d) * (w,x,y,z) = (a*w,b*x,c*y,d*z)
  negate v = (-1,-1,-1,-1) * v
  abs v = v
  signum (0,0,0,0) = 0 -- highest common factor would also satisfy the laws here and could be useful
  signum _ = 1
  fromInteger n = let m = fromInteger n in (m,m,m,m)


instance Num (Int,Int) where
  (a,b) + (w,x) = (a+w,b+x)
  (a,b) * (w,x) = (a*w,b*x)
  negate v = (-1,-1) * v
  abs v = v
  signum (0,0) = 0 -- highest common factor would also satisfy the laws here and could be useful
  signum _ = 1
  fromInteger n = let m = fromInteger n in (m,m)

-- class Vect k v where
--       (****) :: k -> v -> v
--
-- instance Vect Int (Int,Int,Int,Int) where
--       (a,b,c,d) ++++ (w,x,y,z) = (a+w,b+x,c+y,d+z)
--       n **** (l,t,x,y) = (n*l,n*t,n*x,n*y)

white = "♔♕♖♗♘♙"
black = "♚♛♜♝♞♟︎"
