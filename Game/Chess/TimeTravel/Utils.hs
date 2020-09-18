module Game.Chess.TimeTravel.Utils where

import System.IO.Unsafe(unsafePerformIO)

up x y = unsafePerformIO (print x >> return y)

upi = unsafePerformIO

-- from relude
infix 9 !!?
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i
    | i < 0     = Nothing
    | otherwise = go i xs
  where
    go :: Int -> [a] -> Maybe a
    go 0 (x:_)  = Just x
    go j (_:ys) = go (j - 1) ys
    go _ []     = Nothing
{-# INLINE (!!?) #-}


maybeProd :: [Maybe [a]] -> [[a]]
maybeProd [] = [[]]
maybeProd (Nothing:ls) = maybeProd ls
maybeProd ((Just l):ls) = do
  x <- l
  map (x:) (maybeProd ls)

-- maybeProd = foldr (\ ml t -> maybe t (\ l -> l>>=(\ x -> map(x:) t) ml) [[]]

modifyAt :: [a] -> Int -> (a->a) -> [a]
modifyAt (x:xs) 0 f = f x : xs
modifyAt (x:xs) n f = x : modifyAt xs (n-1) f

interleave :: [a] -> [a] -> [a]
interleave (a:as) bs = a:interleave bs as
interleave [] [] = []
interleave [] _ = error "unable to interleave lists due to size mismatch"
