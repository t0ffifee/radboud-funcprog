module Folders where

import Prelude hiding (and,or,elem,maximum)

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

elem :: (Eq a) => a -> [a] -> Bool
elem x = foldr (\y b -> x == y || b) False

maximum :: (Ord a) => [a] -> a
maximum = foldr1 max

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Leaf

fromBits :: [Integer] -> Integer
--fromBits [] = 0
--fromBits (x:xs) = x + 2 * fromBits xs
fromBits = foldr (\x y -> x + (2 * y)) 0

{- -------------------------------------------------------------------}

-- the relevant definitions for 'fromList'

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node key lt rt)
  | x < key   = Node key (insert x lt) rt
  | x > key   = Node key lt (insert x rt)
  | otherwise = tree
