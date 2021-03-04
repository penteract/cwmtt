module Game.Chess.TimeTravel.PGNParser
where
-- notation described by https://github.com/adri326/5dchess-notation
-- ambiguities attempt to follow https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt

import Game.Chess.TimeTravel.Datatypes
import Game.Chess.TimeTravel.Parser(Parser, (<?>), (>>$), ($$)
  , MoveData(..), MoveSetPartial(..), CheckData(..), MoveP(..)
  , ret, unknownPos, parseJT, get, justify, parseFile, parseRank, parseInt, parseNat, parsePiece)
import Data.Char
import qualified Data.Map.Lazy as Map
import Data.List(foldl')
import Data.Maybe (isJust)

data NotatedGame = Notated {
    annotations :: Map.Map String String
  , moves :: [MoveSetPartial]
  , unparsed :: String
} deriving (Show)

combine :: CheckData -> CheckData -> CheckData
combine Nocheck c = c
combine c Nocheck = c
combine (Mate as) (Mate bs) = Mate (as ++ bs)
combine c@(Mate _) _ = c
combine _ c@(Mate _) = c
combine (Check as) (Check bs) = Check (as ++ bs)


skipTo :: Char -> Parser ()
skipTo c (d:rs) = if c==d then Just ((),rs) else skipTo c rs
skipTo c [] = Just ((),[])

-- ignore spacing and comments
eatSpace :: Parser ()
eatSpace "" = Just ((),"")
eatSpace (';':rs) = (skipTo '\n' >>$ const eatSpace) rs
eatSpace ('{':rs) = (skipTo '}' >>$ const eatSpace) rs
eatSpace (c:rs) = if isSpace c then eatSpace rs else Just ((),c:rs)

allowSpace :: Parser a -> Parser a
allowSpace p =
  eatSpace >>$ \ _ -> p

parsePGN :: Parser NotatedGame
parsePGN =
  parseAnnotations >>$ \ annot ->
  parsePGNMoveSets >>$ \ mvs ->
  allowSpace (\ s -> Just (s,"")) >>$ \ rest ->
  ret (Notated annot mvs rest)

parseMultiple :: Parser a -> Parser [a]
parseMultiple p =
  justify p >>$ \ x ->
  case x of
    Nothing -> ret []
    (Just a) -> (a:) $$ parseMultiple p

-- moves
parsePGNMoveSets :: Parser [MoveSetPartial]
parsePGNMoveSets =
  parseMultiple  parsePGNMoveSet
  {- justify parsePGNMoveSet >>$ \ mv ->
  case mv of
    Nothing -> ret []
    (Just a) -> (a:) $$ parsePGNMoveSets -}

parsePGNMoveSet :: Parser MoveSetPartial
parsePGNMoveSet =
  parsePGNSep >>$ \ mspfn ->
  uncurry mspfn $$ parsePGNMoves >>$ \ msp ->
  ignoreExtras >>$ const (ret msp)

ignoreExtras :: Parser ()
ignoreExtras = parseMultiple (reactivation <?> newTL) >>$ \ _ -> ret ()

withSpacing :: [String] -> Parser ()
withSpacing (p:ps) = allowSpace (get p ()) >>$ const (withSpacing ps)
withSpacing [] = ret ()

newTL :: Parser ()
newTL = withSpacing (map (:[]) "(~T" ) >>$ \ _ ->
  allowSpace parseNat >>$ \ _ ->
  allowSpace (get ")" ())
reactivation :: Parser ()
reactivation = withSpacing (map (:[]) "(>L" ) >>$ \ _ ->
  allowSpace parseInt >>$ \ _ ->
  allowSpace (get ")" ())


parsePGNMoves :: Parser ([MoveP], CheckData)
parsePGNMoves =
  allowSpace parsePGNMove >>$ \ m ->
  foldl'  combine Nocheck $$ parseMoveAnnots >>$ \ ch ->
  (get " " () >>$ const parsePGNMoves) <?> ret ([], Nocheck) >>$ \ (ms,ch') -> -- Consider allowing other kinds of space
  ret (m:ms, combine ch ch')

-- T? Piecename? rank? file? Jumpspec? 'x'? T? rank file Checkspec?
parsePGNMove :: Parser MoveP
parsePGNMove =
  parseSuper >>$ \ (l,t) ->
  (parsePiece <?> ret Pawn) >>$ \ p ->
  (justify parseFile >>$ \ x ->
   justify parseRank >>$ \ y ->
   parsePGNMoveNoSource >>$ \ mv ->
   ret (MoveFrom (mv{piece=Just p, source=(l,t,x,y)}))) <?>
  (parsePGNMoveNoSource >>$ \ mv ->
   ret (MoveFrom (mv{piece=Just p, source=(l,t,Nothing,Nothing)})) )

parsePGNMoveNoSource :: Parser MoveData
parsePGNMoveNoSource =
  parseJT >>$ \ jt ->
  (get "x" True <?> ret False) >>$ \ caps ->
  parseSuper >>$ \ (l,t) ->
  parseFile >>$ \ x ->
  parseRank >>$ \ y ->
  ret (MD Nothing unknownPos jt caps (l,t, Just x, Just y) (False,False))

parseSuper :: Parser (Maybe Int, Maybe Int)
parseSuper =
  (get "(" () >>$ \ _ ->
  allowSpace (get "L" ()) <?> ret () >>$ \ _ ->
  allowSpace parseInt >>$ \ l ->
  allowSpace (get "T" ()) >>$ \ _ ->
  allowSpace parseInt >>$ \ t ->
  allowSpace (get ")" ()) >>$ \ _ ->
  ret (Just l, Just t))
  <?> ret (Nothing,Nothing)


parseMoveAnnots :: Parser [CheckData]
parseMoveAnnots = parseMultiple (allowSpace
  (get "+" (Check []) <?> get "#" (Mate []) <?> get "~" Nocheck <?> get "*" (Check [])))

parsePGNSep :: Parser ([MoveP] -> CheckData -> MoveSetPartial)
parsePGNSep =
  justify (allowSpace parsePGNNat) >>$ \ n ->
  justify (allowSpace (get "w" White <?> get "b" Black)) >>$ \ p ->
   (if isJust n || isJust p
     then allowSpace eatDots
     else allowSpace (get "/" ()) {- <?> getNL -}) >>$ \ ()->
  ret (MSP n Nothing p)

eatDots :: Parser ()
eatDots ('.':rs) = eatDots rs
eatDots rs = Just ((),rs)

getNL :: Parser ()
getNL ('\n':rs) = Just ((),rs)
getNL (c:rs) = if isSpace c then getNL rs else Nothing

-- annotations
parseAnnotations :: Parser (Map.Map String String)
parseAnnotations = Map.fromList $$ parseAnnotationsList

parseAnnotationsList :: Parser [(String,String)]
parseAnnotationsList =
  justify parseAnnotation >>$ \ annot ->
  case annot of
    Nothing -> ret []
    (Just an) -> (an:) $$ parseAnnotationsList

parseAnnotation :: Parser (String, String)
parseAnnotation =
  allowSpace (get "[" ()) >>$ \ _ ->
  allowSpace getSymbol >>$ \ sym ->
  allowSpace getString >>$ \ str ->
  allowSpace (get "]" ()) >>$ \ _ ->
  ret (sym,str)

symbolChars = [('a','z'),('A','Z'),('0','9'),('_','_')]

getSymbol :: Parser String
getSymbol (c:rs) =if any (\ (x,y) -> x<=c && c<=y) symbolChars
  then ((c:)$$ getSymbolEnd) rs else Nothing
getSymbol "" = Nothing

getSymbolEnd :: Parser String
getSymbolEnd "" = Just ("","")
getSymbolEnd (c:rs) = if any (\ (x,y) -> x<=c && c<=y) symbolChars
  then ((c:)$$ getSymbolEnd) rs else Just ("",c:rs)


getString :: Parser String
getString = get "\"" () >>$ \ _ -> getStringRec

getStringRec :: Parser String
getStringRec ('\"':rs) = Just ("",rs)
getStringRec ('\\':c:rs) = ((c:) $$ getStringRec) rs
getStringRec (c:rs) = ((c:) $$ getStringRec) rs
getStringRec "" = Nothing


parsePGNNat :: Parser Int
parsePGNNat (d:rs)
  | '0'<=d && d<='9'  =  parsePGNNat1 (fromEnum d - fromEnum '0') rs
  | otherwise  =  Nothing
parsePGNNat [] = Nothing

parsePGNNat1 :: Int -> Parser Int
parsePGNNat1 n (d:rs)
  | '0'<=d && d<='9'  = parsePGNNat1 (n*10 + (fromEnum d - fromEnum '0')) rs
parsePGNNat1 n rs = Just (n,rs)
