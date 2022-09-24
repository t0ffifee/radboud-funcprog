module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

-- sorts a given list, but also
-- records the position each element had in the original list. For example:
-- sortWithPos "haskell" ⟹ [(’a’,1),(’e’,4),(’h’,0),(’k’,3),(’l’,5),(’l’,6),(’s’,2)]

-- step 1: get original position
-- step 2: zip step 1with original list
-- stop 3: sort by first element

-- sort a given list, but also add the position each element had in the original list
sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sort(getIndexesOfString xs 0)

-- get the indexes of a string
getIndexesOfString :: (Ord a) => [a] -> Int -> [(a,Int)]
getIndexesOfString [] _ = []
getIndexesOfString (x:xs) n = (x,n) : getIndexesOfString xs (n+1)

sortedPos :: (Ord a) => [a] -> [(a,Int)]
-- IDEA: we do the same as the above but save the original position first
sortedPos xs = map (removeindex) (sort(map changetuple (sort(xs `zip` [0..]) `zip` [0..])))

-- switch tuple to enable sort
changetuple ((a,b),c) = ((b,a),c)

-- remove sort index after sorting because we don't need it anymore
removeindex ((a,b),c) = (b,c)
