module Game.Chess.TimeTravel.Parser
where

import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Utils

import Control.Applicative
import Control.Arrow
import Data.Char

type Parser a = String -> Maybe (a,String)

data MoveSetPartial = MSP {
    number :: Maybe Int -- number of moves previously played by the player + 1
  , tnumber :: Maybe Int -- T index of the present at start of turn
  , player :: Maybe Player
  , moves :: [MoveP]
  , check :: CheckData
} deriving (Eq,Show)

type PartialPos =  (Maybe Int, Maybe Int, Maybe Int, Maybe Int)

unknownPos :: PartialPos
unknownPos = (Nothing,Nothing,Nothing,Nothing)

data JumpType = SingleBoard | Hopping | Branching deriving (Eq,Show)

data MoveP = Pass | NotTurn | MoveFrom MoveData | Arrive deriving (Eq,Show)

data MoveData = MD {
    piece :: Maybe Piece
  , source :: PartialPos
  , jumpType :: JumpType
  , captures :: Bool
  , destination :: PartialPos
  , relative :: (Bool,Bool)
} deriving (Eq,Show)

data CheckData = Nocheck | Check [(Maybe Piece,PartialPos)] | Mate [(Maybe Piece,PartialPos)] deriving (Eq,Show)

justify :: Parser a -> Parser (Maybe a)
justify p s = case p s of
  Just (x,s') -> Just (Just x, s')
  Nothing -> Just (Nothing, s)

-- If only Maybe had the right instance of semigroup
(<?>) :: Parser a -> Parser a -> Parser a
(pa <?> pb) s = pa s <|> pb s

(>>$) :: Parser a -> (a -> Parser b) -> Parser b
(xm >>$ f) s = xm s >>= (\ (x,rs) -> f x rs)

($$) :: (a -> b) -> Parser a -> Parser b
(f $$ p) s = first f <$> p s

parseFail :: Parser a
parseFail s = Nothing

ret :: a -> Parser a
ret x s = Just (x,s)

get :: String -> a -> Parser a
get "" x rs = Just (x,rs)
get _ x "" = Nothing
get (a:as) x (b:bs) = if a==b then get as x bs else Nothing

-- remove non newline whitespace and comments
preproc :: String -> String
preproc [] = []
preproc ('?':rs) = skipToNL rs
  where skipToNL ('\n':rs') = '\n':preproc rs'
        skipToNL (_:rs') = skipToNL rs'
        skipToNL [] = []
preproc (c:rs)
  | isSpace c && c/='\n'  =  preproc rs
  | otherwise  =  c:preproc rs

-- throws an error on parse failure
getTurnSequence :: String -> Result String MoveSetPartial
getTurnSequence s = case (parseTurnSeparator >>$ const parseTurn) ('\n':s) of
  Just (msp,rs) -> Cons msp (getTurnSequence rs)
  Nothing -> case eatNewLines s of
    Just ((),"") -> Nil
    Just ((),rs) -> Fail ("Parsing failed - next 20 characters: " ++ show (take 20 rs))

parseTurnSeparator :: Parser ()
parseTurnSeparator ('\n':rs) = parseTurnSeparator1 rs
parseTurnSeparator ('/':rs) = parseTurnSeparator1 rs
parseTurnSeparator _ = Nothing
parseTurnSeparator1 :: Parser ()
parseTurnSeparator1 ('\n':rs) = parseTurnSeparator1 rs
parseTurnSeparator1 ('/':rs) = parseTurnSeparator2 rs
parseTurnSeparator1 rs = Just ((), rs)
parseTurnSeparator2 :: Parser ()
parseTurnSeparator2 ('\n':rs) = parseTurnSeparator2 rs
parseTurnSeparator2 rs = Just ((), rs)

parseTurn :: Parser MoveSetPartial
parseTurn = (parseTurnNumber <?> ret (MSP Nothing Nothing Nothing)) >>$ \ f ->
  parseMoveList >>$ \ mvs ->
  parseCheck >>$ \ chk ->
  ret (f mvs chk)

parseTurnNumber :: Parser ([MoveP] -> CheckData -> MoveSetPartial)
parseTurnNumber = justify parseNat >>$ \ n ->
  justify (get "w" White <?> get "b" Black) >>$ \ p ->
  parseT >>$ \ (t,trel) ->
  if trel then parseFail else
  (if (n,p,t)==(Nothing,Nothing,Nothing) then ret else get ".") (MSP n t p)

parseMoveList :: Parser [MoveP]
parseMoveList = parseMove >>$ (\ mv ->
  ((parseMoveSep >>$ const parseMoveList)
  <?> ret []) >>$ (\ mvs ->
  ret (mv : mvs) ))

parseMoveSep :: Parser ()
parseMoveSep = eatNewLines >>$ const (get ";" () >>$ const eatNewLines)

eatNewLines :: Parser ()
eatNewLines ('\n':rs) = eatNewLines rs
eatNewLines rs = Just ((),rs)

parseMove :: Parser MoveP
parseMove = (MoveFrom $$ parseMoveData)
  <?> get "-" Pass
  <?> get "<" Arrive
  <?> get "_" NotTurn

-- Move = Sourceposition? Piecename? Jumpspec? 'x'? Destposition Checkspec?
parseMoveData :: Parser MoveData
parseMoveData = parseMoveWithSource <?> parseMoveNoSource

parseMoveWithSource :: Parser MoveData
parseMoveWithSource =  parseSource >>$ \ pos ->
    parseMoveNoSource >>$ \ md -> ret md{source=pos}

parseMoveNoSource :: Parser MoveData
parseMoveNoSource = (parsePiece <?> ret Pawn) >>$ \ p ->
  parseJT >>$ \ jt ->
  get "x" True <?> ret False >>$ \ caps ->
  parseDest >>$ \ (pos,rel) ->
  ret (MD (Just p) unknownPos jt caps pos rel)

parseCheck :: Parser CheckData
parseCheck = get "+" (Check []) <?> get "#" (Mate []) <?> ret Nocheck

parseJT :: Parser JumpType
parseJT = get ">>" Branching <?> get ">" Hopping <?> ret SingleBoard


-- Destposition = (n'L'|'l')?('T'|'t'n)? File Rank
parseDest :: Parser (PartialPos,(Bool,Bool))
parseDest = parseL >>$ \ (l,lrel) ->
  parseT >>$ \ (t,trel) ->
  parseFile >>$ \ x ->
  parseRank >>$ \ y ->
  ret ((l,t,Just x,Just y),(lrel,trel))

--
parseL :: Parser (Maybe Int, Bool)
parseL = (parseInt >>$ \ l ->
  (get "l" True <?> get "L" False) >>$ \ lrel ->
  ret (Just l, lrel)
  ) <?> ret (Nothing,False)
parseT :: Parser (Maybe Int, Bool)
parseT = (
  (get "t" True <?> get "T" False) >>$ \ trel ->
  parseInt >>$ \ t ->
  ret (Just t, trel)
  ) <?> ret (Nothing,False)


-- Sourceposition = (n'L'|'l')?('T'|'t'n)? File? Rank?
parseSource :: Parser PartialPos
parseSource = parseL >>$ \ (l,lrel) ->
  if lrel then parseFail else
  parseT >>$ \ (t,trel) ->
  if trel then parseFail else
  justify parseFile >>$ \ x ->
  justify parseRank >>$ \ y ->
  ret (l,t,x,y)

parseFile :: Parser Int
parseFile [] = Nothing
parseFile (f:rs)
  | 'a'<=f && f<'t'  =  Just (fromEnum f - fromEnum 'a', rs)
  | otherwise  =  Nothing

parseRank :: Parser Int
parseRank = parseNat >>$ (\ x s -> if x==0 then Nothing else Just (x-1,s))

parsePiece :: Parser Piece
parsePiece ('K':rs) = Just (King,rs)
parsePiece ('N':rs) = Just (Knight,rs)
parsePiece ('B':rs) = Just (Bishop,rs)
parsePiece ('R':rs) = Just (Rook,rs)
parsePiece ('Q':rs) = Just (Queen,rs)
parsePiece ('P':rs) = Just (Pawn,rs)
parsePiece ('U':rs) = Just (Unicorn,rs)
parsePiece ('D':rs) = Just (Dragon,rs)
parsePiece _ = Nothing

-- leading 0s not allowed
parseInt :: Parser Int
parseInt ('-':n) = first negate <$> parseNat n
parseInt ('+':n) = parseNat n
parseInt n = parseNat n

parseNat :: Parser Int
parseNat (d:rs)
  | '1'<=d && d<='9'  =  parseNat1 (fromEnum d - fromEnum '0') rs
  | d=='0'  =  Just (0, rs)
  | otherwise  =  Nothing
parseNat [] = Nothing

parseNat1 :: Int -> Parser Int
parseNat1 n (d:rs)
  | '0'<=d && d<='9'  = parseNat1 (n*10 + (fromEnum d - fromEnum '0')) rs
parseNat1 n rs = Just (n,rs)

-- TODO: consider +0L and -0L distinct
