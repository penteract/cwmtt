{-# LANGUAGE BlockArguments #-}
module Game.Chess.TimeTravel.SAT(SearchSpace, Sec, XSec, toFormula, exactlyOne, mkSpace, addAssertion, all', getPoint)
where

import Game.Chess.TimeTravel.Utils
--import Data.Boolean.SatSolver
import Z3.Base
import Data.IntMap(IntMap)
import Data.List(foldl')
import Control.Monad
import GHC.IO.Unsafe(unsafePerformIO)

type SearchSpace = (Int, Context, Solver)

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
fresh = M \n -> (Lit n,n+1,[])

toCNF :: Prop -> M [[Prop]]
toCNF (Conj xs) = concat <$> mapM toCNF xs
toCNF (Disj ys) = (:[]) <$> mapM makeVar ys
toCNF t@(Lit x) = return [[t]]
toCNF t@(Neg (Lit x)) = return [[t]]
toCNF _ = error "unsimplified"

makeVar :: Prop -> M Prop
makeVar t@(Lit x) = return t
makeVar t@(Neg (Lit x)) = return t
makeVar (Conj xs) = do
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
mkSpace hc = unsafePerformIO $ do
  ctx <- mkContext =<< mkConfig
  slv <- mkSolver ctx
  return (10000,ctx,slv)

addAssertion :: Sec -> SearchSpace -> SearchSpace
addAssertion b (n,ctx,slv) = unsafePerformIO $ do
  --let (cs,m,helper) = runM (toCNF.group.pushNeg$b) n
  constr <- propToBoolean ctx b -- toBoolean ctx (cs ++ helper)
  solverAssertCnstr ctx slv constr
  return (n,ctx,slv)
  {-let (cs,m,helper) = runM (toCNF.group.pushNeg$b) n
      in (m, (s >>= assertTrue (toBoolean (cs ++ helper)) ))-}

getPoint :: [(Int,[(Int,a)])] -> Int -> SearchSpace -> (Maybe [(Int,a)], SearchSpace)
getPoint spc i (n,ctx,slv) = unsafePerformIO $ do
  (r, m') <- solverCheckAndGetModel ctx slv
  case r of
    Unsat -> return (Nothing,(n,ctx,slv))
    Sat -> do
      let Just m = m'
      firsts <- mapM (firstInRow ctx m) spc
      return (Just firsts,(n,ctx,slv))
  where
    firstInRow :: Context -> Model -> (Int,[(Int,a)]) -> IO (Int,a)
    firstInRow ctx m (l,(x,p):xs) = do
      v <- evalBool ctx m  =<< litToBoolean ctx (toVar i l x)
      if (v==Just True) then return (x,p)
        else firstInRow ctx m (l,xs)
    firstInRow ctx m (l,[]) = error ("bad"++show l)
  {-case ss >>= solve of
   [] -> Nothing
   (solved:_) -> Just [ head [(x,p) | (x,p)<-ax, Just True<-[lookupVar (toVarNum i l x) solved]] | (l,ax) <- spc]-}


toBoolean :: Context -> [[Prop]] -> IO AST
toBoolean ctx = mkAnd ctx <=< mapM (mkOr ctx <=< mapM (litToBoolean ctx))

litToBoolean :: Context -> Prop -> IO AST
litToBoolean ctx (Lit n) = mkIntSymbol ctx n >>= mkBoolVar ctx
litToBoolean ctx (Neg (Lit n)) = mkNot ctx =<< litToBoolean ctx (Lit n)

propToBoolean :: Context -> Prop -> IO AST
propToBoolean ctx (Lit n) = litToBoolean ctx (Lit n)
propToBoolean ctx (Neg p) = mkNot ctx =<< propToBoolean ctx p
propToBoolean ctx (Conj xs) = mkAnd ctx =<< mapM (propToBoolean ctx) xs
propToBoolean ctx (Disj xs) = mkOr ctx =<< mapM (propToBoolean ctx) xs
