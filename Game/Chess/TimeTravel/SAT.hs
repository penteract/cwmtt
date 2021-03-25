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

type SearchSpace = (Context, Solver)

type Sec = Prop
type XSec = [(Int,[Int])]

withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)
    where
        withoutEach' f (xs,x,[]) = [f (reverse xs) x]
        withoutEach' f (xs,x,y:ys) = f (reverse xs ++ (y:ys)) x : withoutEach' f (x:xs,y,ys)


data Prop = Conj [Prop] | Disj [Prop] | Neg Prop | Lit Int deriving (Eq,Show)

any' :: [Prop] -> Prop
any' = Disj
all' :: [Prop] -> Prop
all' = Conj

mkAx :: Int -> (Int,[Int]) -> Prop
mkAx i (l,xs) = any' [toVar i l x | x <- xs]

toFormula :: Int -> XSec -> Sec
toFormula i xsec = Neg (all' [any' [toVar i l x | x <- sec]  | (l,sec) <- xsec])

toVar :: Int -> Int -> Int -> Prop
toVar i l x = Lit (toVarNum i l x)

toVarNum :: Int -> Int -> Int -> Int
toVarNum i l x = l + x * i

exactlyOne :: Int -> [(Int,[Int])] -> Sec
exactlyOne i xs =
  (any' $ withoutEach (\ rest x ->  all' (mkAx i x : [Neg (mkAx i r) | r <- rest])) xs)

mkSpace :: [[(Int,a)]] -> SearchSpace
mkSpace hc = unsafePerformIO $ do
  ctx <- mkContext =<< mkConfig
  slv <- mkSolver ctx
  return (ctx,slv)

addAssertion :: Sec -> SearchSpace -> SearchSpace
addAssertion b (ctx,slv) = unsafePerformIO $ do
  constr <- propToBoolean ctx b
  solverAssertCnstr ctx slv constr
  return (ctx,slv)

getPoint :: [(Int,[(Int,a)])] -> Int -> SearchSpace -> (Maybe [(Int,a)], SearchSpace)
getPoint spc i (ctx,slv) = unsafePerformIO $ do
  (r, m') <- solverCheckAndGetModel ctx slv
  case r of
    Unsat -> return (Nothing,(ctx,slv))
    Sat -> do
      let Just m = m'
      firsts <- mapM (firstInRow ctx m) spc
      return (Just firsts,(ctx,slv))
  where
    firstInRow :: Context -> Model -> (Int,[(Int,a)]) -> IO (Int,a)
    firstInRow ctx m (l,(x,p):xs) = do
      v <- evalBool ctx m  =<< litToBoolean ctx (toVar i l x)
      if (v==Just True) then return (x,p)
        else firstInRow ctx m (l,xs)
    firstInRow ctx m (l,[]) = error ("bad"++show l)

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
