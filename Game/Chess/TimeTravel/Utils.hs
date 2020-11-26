module Game.Chess.TimeTravel.Utils where

import System.IO.Unsafe(unsafePerformIO)
import System.IO(hPutStrLn,stderr)

up x y = unsafePerformIO (print x >> return y)
upp x = up x x

upi = unsafePerformIO

warn msg val = unsafePerformIO$ do
  hPutStrLn stderr msg
  return val

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



data Result e a = Cons a (Result e a) | Nil | Fail e

rscanl :: (b -> a -> Either e b) -> b -> [a] -> Result e b
rscanl f s xs = Cons s $ case xs of
  [] -> Nil
  (x:xs') -> case f s x of
    Left e -> Fail e
    Right s' -> rscanl f s' xs'

rToEList :: Result e a -> Either e [a]
rToEList Nil = Right []
rToEList (Fail e) = Left e
rToEList (Cons x mxs) = (x:) <$> rToEList mxs


rToList :: Result e a -> [a]
rToList Nil = []
rToList (Fail e) = []
rToList (Cons x mxs) = x : rToList mxs

-- | If an rList ends in failure, emit a warning when that failure is reached
-- impure
rToListWarn :: Result String a -> [a]
rToListWarn Nil = []
rToListWarn (Fail e) = warn e []
rToListWarn (Cons x mxs) = x : rToListWarn mxs

rmap :: (a->b) -> Result e a -> Result e b
rmap f (Cons x xs) = Cons (f x) (rmap f xs)
rmap f (Fail e) = Fail e
rmap f Nil = Nil

rEnd :: Result e a -> Result e b
rEnd (Cons x xs) = rEnd xs
rEnd Nil = Nil
rEnd (Fail e) = Fail e

-- Propogate an eventual failure through a function that accepts a list
rDelistify :: ([a] -> Result e b) -> Result e a -> Result e b
rDelistify f r = rDelistify' (f (rToList r))
  where
    rDelistify' (Fail e) = Fail e
    rDelistify' (Cons x xs) = Cons x (rDelistify' xs)
    rDelistify' Nil = rEnd r


instance Functor (Result e) where
  fmap = rmap

infixl 1 +?
(+?):: Monoid e => e -> Either e a -> Either e a
e' +? Left e = Left (e'<>e)
_ +? Right x = Right x

(?->) :: Maybe a -> (a -> e) -> Either e ()
Nothing ?-> f = Right ()
Just x ?-> f = Left (f x)

infixl 1 ?
(?) :: Maybe a -> e -> Either e a
Nothing ? e = Left e
Just x ? e = Right x
