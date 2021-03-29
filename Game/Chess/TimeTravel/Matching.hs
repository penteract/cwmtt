module Game.Chess.TimeTravel.Matching(constructMatching) where

import Data.IntMap hiding(map,filter,foldl')
import qualified Data.IntMap as M
import Data.List(foldl')
import qualified Data.IntSet as IS
import Game.Chess.TimeTravel.Utils
import Control.Arrow
import GHC.Stack (HasCallStack)
import Data.Maybe(fromJust,fromMaybe)

type Graph = IntMap [Int]

constructMatching :: HasCallStack => [(Int,(Int,a))] -> [Int] -> [(Int,a)] -> Maybe [(Int,a)]
constructMatching edges include omittable = let
    --g :: IntMap [(Int,a)]
    --insertA :: IntMap a -> (Int,a) -> IntMap a
    -- insertA mp (x,p) = insert x p mp
    --gmp = foldl' (flip $ uncurry$ insertWith (\ [x] y -> if fst x `elem` map fst y then y else (x:y)) ) empty (map (second (:[])) edges)
    gmp = foldl' (flip $ uncurry$ insertWith union ) empty (map (second (uncurry singleton)) edges)
    g = M.map keys gmp
    --omtbl = fromList omittable

    in case matchg g include of
      Nothing -> Nothing
      Just m -> Just (map (\(a,b)->(a,fromJust((gmp!?a)>>=(!?b)))) (toAscList m) ++ [(a,x) | (a,x) <- omittable, a `notMember` m])

-- match :: [(Int,[Int])] -> [Int] -> Gra [(Int,Int)]
-- match = matchg . fromList

-- | Find a matching in 'g' that includes all vertices in 'include'
matchg :: Graph -> [Int] -> Maybe (IntMap Int)
matchg g include = match' g include (IS.fromList include) empty

type Set = IS.IntSet

match' :: Graph -> [Int] -> Set -> IntMap Int -> Maybe (IntMap Int)
match' g [] mustinc mtch = Just mtch
match' g (v:vs) mustinc mtch = match' g vs mustinc =<< if v `member` mtch then Just mtch
  else case bfs g mustinc mtch v of
    (Just (Left (v:p))) -> Just (delete v (updateAll p mtch))
    (Just (Right p)) -> Just (updateAll p mtch)
    Nothing -> Nothing

updateAll :: [Int] -> IntMap Int -> IntMap Int
updateAll (v:u:p) = updateAll p . insert v u . insert u v
updateAll [] = id
updateAll [x] = error "err"

bfs :: Graph -> Set -> IntMap Int -> Int -> Maybe (Either [Int] [Int])
bfs g mustinc mtch v = bfs' (map (:[v]) (fromMaybe [] (g !? v))) (IS.fromList (v :fromMaybe [] (g!?v))) []
  where
    bfs' :: [[Int]] -> Set -> [[Int]] -> Maybe (Either [Int] [Int])
    bfs' (p:ps) seen next = case mtch !? head p of
      (Just v) -> if IS.member v mustinc
         then let
           us = filter (`IS.notMember` seen) (g!v)
           nextp = v:p
           seen' = foldl' (flip IS.insert) seen us
           in bfs' ps seen' (map (:nextp) us ++ next)
         else Just (Left (v:p))
      Nothing -> Just (Right p)
    bfs' [] seen [] = Nothing
    bfs' [] seen next = bfs' next seen []
