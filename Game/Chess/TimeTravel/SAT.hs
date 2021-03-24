{-# LANGUAGE BlockArguments #-}
module Game.Chess.TimeTravel.SAT(SearchSpace, Sec, XSec, toFormula, exactlyOne, mkSpace, addAssertion, all', getPoint)
where

import Game.Chess.TimeTravel.Utils
import Data.Boolean.SatSolver
import Data.List(foldl')

type SearchSpace = (Int, [SatSolver])

type Sec = Prop
type XSec = [(Int,[Int])]

withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)
    where
        withoutEach' f (xs,x,[]) = [f (reverse xs) x]
        withoutEach' f (xs,x,y:ys) = f (reverse xs ++ (y:ys)) x : withoutEach' f (x:xs,y,ys)


data Prop = Conj [Prop] | Disj [Prop] | Neg Prop | Lit Int deriving (Eq,Show)

neg (Neg x) = x
neg x = Neg x

-- push negations to the bottom
pushNeg :: Prop -> Prop
pushNeg (Neg (Conj xs)) = Disj (map (pushNeg. Neg) xs)
pushNeg (Neg (Disj xs)) = Conj (map (pushNeg. Neg) xs)
pushNeg (Neg (Neg x)) = pushNeg x
pushNeg t@(Neg (Lit x)) = t
pushNeg t@(Lit x) = t
pushNeg (Conj xs) = Conj (map pushNeg xs)
pushNeg (Disj xs) = Disj (map pushNeg xs)

-- group conjunctions and disjunctions. Should be called after pushNeg
group :: Prop -> Prop
group (Conj [x]) = group x
group (Disj [x]) = group x
group (Conj xs) = Conj (map group xs >>= liftConj)
  where
    liftConj (Conj xs) = xs
    liftConj x = [x]
group (Disj xs) = Disj (map group xs >>= liftDisj)
  where
    liftDisj (Disj xs) = xs
    liftDisj x = [x]
group x = x

newtype M a = M {runM :: Int -> (a,Int,[[Prop]])}
instance Functor M where
  fmap f xm = M \ n -> let (x,m,ps) = runM xm n in (f x,m,ps)
instance Applicative M where
  pure x = M \ n -> (x,n,[])
  (<*>) fm xm = M \ n -> let (f,m,ps) = runM fm n in let (x, m',qs) = runM xm m in (f x , m', qs++ps)
instance Monad M where
  xm >>= g = M \ n -> let (x,m,ps) = runM xm n in let (y, m',qs) = runM (g x) m in (y, m', qs++ps)

fresh :: M Prop
fresh = M \n -> (Lit n,n-1,[])

toCNF :: Prop -> M [[Prop]]
toCNF (Conj xs) = concat <$> mapM toCNF xs
toCNF (Disj ys) = (:[]) <$> mapM mkVar ys
toCNF t@(Lit x) = return [[t]]
toCNF t@(Neg (Lit x)) = return [[t]]
toCNF _ = error "unsimplified"

mkVar :: Prop -> M Prop
mkVar t@(Lit x) = return t
mkVar t@(Neg (Lit x)) = return t
mkVar (Conj xs) = do
  cnf <- toCNF (Conj xs)
  f <- fresh
  M (\ n ->  ((),n, map (neg f:) cnf))
  --mapM_ (\ dj -> addClause (neg f:dj)) cnf
  return f

addClause :: [Prop] -> M ()
addClause c = M \n -> ((),n,[c])

any' :: [Prop] -> Prop
any' = Disj
all' :: [Prop] -> Prop
all' = Conj
{-
any' :: [Boolean] -> Prop
any' [x] = x
any' xs = foldl' (:||:) No xs
all' :: [Boolean] -> Boolean
all' [x] = x
all' xs = foldl' (:&&:) Yes xs
-}

mkAx :: Int -> (Int,[Int]) -> Prop
mkAx i (l,xs) = any' [toVar i l x | x <- xs]

toFormula :: Int -> XSec -> Sec
toFormula i xsec = neg (all' [any' [toVar i l x | x <- sec]  | (l,sec) <- xsec])

toVar :: Int -> Int -> Int -> Prop
toVar i l x = Lit (toVarNum i l x)

toVarNum :: Int -> Int -> Int -> Int
toVarNum i l x = l + x * i

exactlyOne :: Int -> [(Int,[Int])] -> Sec
exactlyOne i xs =
  (any' $ withoutEach (\ rest x ->  all' (mkAx i x : [neg (mkAx i r) | r <- rest])) xs)
  --Not (all' [any' [toVar i l x | x <- sec]  | (l,sec) <- xs]) :&&: all' (withoutEach (\ rest x -> [] :&&: ) xs)
  -- [[any' [toVar i l x | x <- sec]  | (l,sec) <- xs]]

mkSpace :: [[(Int,a)]] -> SearchSpace
mkSpace hc =  (-1,return newSatSolver)

addAssertion :: Sec -> SearchSpace -> SearchSpace
addAssertion b (n,s) = let (cs,m,helper) = runM (toCNF.group.pushNeg$b) n
      in (m, (s >>= assertTrue (toBoolean (cs ++ helper)) ))

getPoint :: [(Int,[(Int,a)])] -> Int -> SearchSpace -> Maybe [(Int,a)]
getPoint spc i (_,ss) = case ss >>= solve of
   [] -> Nothing
   (solved:_) -> Just [ head [(x,p) | (x,p)<-ax, Just True<-[lookupVar (toVarNum i l x) solved]] | (l,ax) <- spc]


toBoolean :: [[Prop]] -> Boolean
toBoolean = foldl' (:&&:) Yes . map (foldl' (:||:) No . map litToBoolean)

litToBoolean :: Prop -> Boolean
litToBoolean (Lit n) = Var n
litToBoolean (Neg (Lit n)) = Not (Var n)
