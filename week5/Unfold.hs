module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- define all the below funtions using `unfoldr`
bits :: Int -> [Int]
bits = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2))

--zip :: [a] -> [b] -> [(a,b)]
--zip [] _ = []
--zip _ [] = []
--zip (x:xs) (y:ys) = (x,y) : zip xs ys


take :: Int -> [a] -> [a]
-- use unfoldr
take n = unfoldr (\(x:xs) -> if n == 0 then Nothing else Just (x, xs))

-- primes :: [Integer]
-- alternative implementation of `primes`:
primes' = sieve [2..]
  where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p /= 0 ]

-- impelement this with `unfoldr`
primes :: [Integer]
primes = unfoldr (\(x:xs) -> Just (x, [ n | n <- xs, n `mod` x /= 0 ])) [2..]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) x y = apo f(x,y)
    where
        f :: ([a],[a]) -> Either [a] (a,([a],[a]))
        f([],y) = Left y
        f(x:xs, y) = Right(x,(xs, y))

insert :: (Ord a) => a -> [a] -> [a]
insert y x = apo f(y,x)
  where
    f(y,[]) = Left [y]
    f(y, x:xs)
     | y>=x = Right (x,(y,xs))
     | otherwise = Left (y:x:xs)

unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo f seed = apo (\s -> case f s of
                                 Nothing -> Left []
                                 Just (a,ns) -> Right (a,ns)) seed

-- Optional!
apoUnfoldr :: (t -> Either [a] (a, t)) -> t -> [a]
apoUnfoldr f seed = unfoldr (\s -> case f s of
                                     Left l -> Nothing
                                     Right (a,ns) -> Just (a,ns)) seed

