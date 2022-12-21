module HOF where

import Prelude hiding (const)

{- exercise 5.1 -}

const :: a -> b -> a
const x y   = x

x $-> y      = y x

oper :: Fractional a => String -> a -> a -> a
oper "mul" n = (*n)
oper "div" n = (n/)
oper _     _ = error "not implemented"

mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f xs  = map (map f) xs

without :: (a -> Bool) -> [a] -> [a]
without p    = filter (not . p)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y   = f (g x) (g y)

{- exercise 5.2 -}

f1 :: (Num a) => a -> a
f1 = (* 5) . (+ 1)

-- f1 x = 5(x+1)

f2 :: (Num a) => a -> a
f2 = (+ 1) . (* 5)

-- f2 x = (x*5)+1

f3 :: (Num a, Ord a) => a -> a
f3 = (min 100) . (max 0)

--f3 = min((max 0 x), 100)
-- always returns at least 0 and at most 100

f4 :: [a] -> Bool
f4 = (<5) . length

-- f4 = (<5) (length x)
-- returns true if length of x is less than 5
-- returns false otherwise

-- if we change it to (5<) . length, it will return true if length of x is greater than 5
-- if we change it to ((<)5) . length, it will return true if length of x is less than 5

