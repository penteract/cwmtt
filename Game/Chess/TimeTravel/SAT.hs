{-# LANGUAGE BlockArguments #-}
module Game.Chess.TimeTravel.SAT(SearchSpace, Sec, XSec, toFormula, exactlyOne, atLeastOne, mkSpace, addAssertion, all', getPoint)
where

import Game.Chess.TimeTravel.Utils
--import Data.Boolean.SatSolver
import Z3.Base
import Data.IntMap(IntMap)
import Data.List(foldl')
import Control.Monad
import GHC.IO.Unsafe(unsafePerformIO)
import Data.Maybe (fromJust)

type SearchSpace = (Context, Solver)

type Sec = Prop
type XSec = [(Int,[Int])]

withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)
    where
        withoutEach' f (xs,x,[]) = [f (reverse xs) x]
        withoutEach' f (xs,x,y:ys) = f (reverse xs ++ (y:ys)) x : withoutEach' f (x:xs,y,ys)


data Prop = Conj [Prop] | Disj [Prop] | ExactlyOne [Prop] | Neg Prop | Lit (Int,Int) deriving (Eq,Show)

any' :: [Prop] -> Prop
any' = Disj
all' :: [Prop] -> Prop
all' = Conj

mkAx :: (Int,[Int]) -> Prop
mkAx (l,xs) = any' [toVar l x | x <- xs]

toFormula :: XSec -> Sec
toFormula xsec = Neg (all' [any' [toVar l x | x <- sec]  | (l,sec) <- xsec])

toVar :: Int -> Int -> Prop
toVar l x = Lit (l, x)

exactlyOne :: [(Int,[Int])] -> Sec
exactlyOne xs =
  ExactlyOne (map mkAx xs)
  -- (any' $ withoutEach (\ rest x ->  all' (mkAx x : [Neg (mkAx r) | r <- rest])) xs)

atLeastOne :: [(Int,[Int])] -> Sec
atLeastOne xs = any' (map mkAx xs)

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
      firsts <- mapM (\ (l,xs) ->
        ((xs!!).fromInteger.fromJust) <$> (evalInt ctx m =<< (mkIntSymbol ctx l >>= mkIntVar ctx)) ) spc
      return (Just firsts,(ctx,slv))

toBoolean :: Context -> [[Prop]] -> IO AST
toBoolean ctx = mkAnd ctx <=< mapM (mkOr ctx <=< mapM (litToBoolean ctx))

litToBoolean :: Context -> Prop -> IO AST
litToBoolean ctx (Lit (l,x)) = do
  v_l <- mkIntSymbol ctx l >>= mkIntVar ctx
  c_x <- mkIntNum ctx x
  mkEq ctx v_l c_x
litToBoolean ctx (Neg (Lit n)) = mkNot ctx =<< litToBoolean ctx (Lit n)

propToBoolean :: Context -> Prop -> IO AST
propToBoolean ctx (Lit n) = litToBoolean ctx (Lit n)
propToBoolean ctx (Neg p) = mkNot ctx =<< propToBoolean ctx p
propToBoolean ctx (Conj xs) = mkAnd ctx =<< mapM (propToBoolean ctx) xs
propToBoolean ctx (Disj xs) = mkOr ctx =<< mapM (propToBoolean ctx) xs
propToBoolean ctx (ExactlyOne xs) = do
  intsort <- mkIntSort ctx
  c0 <- mkInt ctx 0 intsort
  c1 <- mkInt ctx 1 intsort
  xs' <- mapM (propToBoolean ctx) xs
  mkEq ctx c1 =<< mkAdd ctx =<< mapM (\ b -> mkIte ctx b c1 c0) xs'
