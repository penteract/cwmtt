module Game.Chess.TimeTravel.BFS where
import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Moves
import Game.Chess.TimeTravel.Utils()

data GameTree a = GNode State [(MoveSet,GameTree a)] | Leaf a
data Void

isCheck :: State -> Bool
isCheck s = isKnownCheck s' Nothing
  where
    nactive = numActive s
    p = present s
    passes = [[((l,t,0,0),(l,t,0,0))] | (l,t) <- playableBoards s, abs l<=nactive, t==p]
    s' = apply passes s

toTree :: (State -> [MoveSet]) -> State -> GameTree Void
toTree fn s = GNode s [(m,toTree fn (apply m s)) | m <- fn s]

data GResult = WinW | Undec | Draw | WinB | S GResult deriving (Eq, Show)
type GR = GResult

pick :: Player -> GR -> GR -> GR
pick White WinW _ = WinW
pick White WinB x = x
pick Black WinB _ = WinB
pick Black WinW x = x -- after this, we put the second in head normal form
pick White _ WinW = WinW
pick White x WinB = x
pick Black _ WinB = WinB
pick Black x WinW = x -- after this, neither argument is WinW or WinB
pick _ Draw Draw = Draw
pick _ Draw Undec = Undec -- draw and undec is at worst Draw, but "at worst" changes by player
pick _ Undec Undec = Undec
pick _ Undec Draw = Undec -- draw and undec is at worst Draw, but "at worst" changes by player
pick c (S a) (S b) = S (pick c a b)
pick c x (S a) = S (pick c x a)
pick c (S a) x = S (pick c a x)

winner :: Player -> GR
winner Black = WinW
winner White = WinB

search ::  Int -> GameTree Void -> GResult
search 0 (GNode s@(_,_,_,p) nds) = if null nds then if isCheck s then winner p else Draw else Undec
search n (GNode s@(_,_,_,p) nds) = if null nds then if isCheck s then winner p else Draw
    else S $foldr1 (pick p) (map (search (n-1) . snd) nds)

deepen :: (State -> [MoveSet]) -> State -> IO ()
deepen fn s = loop 0 (toTree fn s)
  where
    loop n gt = do
      let r = search n gt
      print n
      print r
      case get r of
        Undec -> loop (n+1) gt
        _ -> return ()
    get (S x) = get x
    get x = x
