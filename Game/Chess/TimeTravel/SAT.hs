{-# LANGUAGE BlockArguments #-}
module Game.Chess.TimeTravel.SAT(SearchSpace, Sec, Prop(..), mkSpace, addAssertion, getPoint)
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
-- type XSec = [(Int,[Int])]

withoutEach :: ([a] -> a -> b) -> [a] -> [b]
withoutEach f [] = []
withoutEach f (x:xs) = withoutEach' f ([],x,xs)
    where
        withoutEach' f (xs,x,[]) = [f (reverse xs) x]
        withoutEach' f (xs,x,y:ys) = f (reverse xs ++ (y:ys)) x : withoutEach' f (x:xs,y,ys)


data Prop = All [Prop] | Any [Prop] | ExactlyOne [Prop] | Iff Prop Prop  | J Int Int |  Not Prop | Sec (Int,[Int]) deriving (Eq,Show)


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

litToBoolean :: Context -> Prop -> IO AST
litToBoolean ctx (Sec (l,xs)) = do
  v_l <- mkIntSymbol ctx l >>= mkIntVar ctx
  c_xs <- mapM (mkIntNum ctx) xs
  mkOr ctx =<< mapM (mkEq ctx v_l) c_xs

propToBoolean :: Context -> Prop -> IO AST
propToBoolean ctx (Sec n) = litToBoolean ctx (Sec n)
propToBoolean ctx (Not p) = mkNot ctx =<< propToBoolean ctx p
propToBoolean ctx (All xs) = mkAnd ctx =<< mapM (propToBoolean ctx) xs
propToBoolean ctx (Any xs) = mkOr ctx =<< mapM (propToBoolean ctx) xs
propToBoolean ctx (ExactlyOne xs) = do
  intsort <- mkIntSort ctx
  c0 <- mkInt ctx 0 intsort
  c1 <- mkInt ctx 1 intsort
  xs' <- mapM (propToBoolean ctx) xs
  mkEq ctx c1 =<< mkAdd ctx =<< mapM (\ b -> mkIte ctx b c1 c0) xs'
propToBoolean ctx (J ax ax') = do
  intsort <- mkIntSort ctx
  cax' <- mkInt ctx ax' intsort
  v <- mkIntVar ctx =<< mkStringSymbol ctx (show ax)
  mkEq ctx v cax'
propToBoolean ctx (Iff p1 p2) = do
  x1 <- propToBoolean ctx p1
  x2 <- propToBoolean ctx p2
  mkIff ctx x1 x2
