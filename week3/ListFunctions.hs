module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem x (y:ys) = x == y || elem x ys

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs 

take :: Int -> [a] -> [a]
take _ [] = []
take 0 _ = []
take n (x:xs) = x : take (n-1) xs   