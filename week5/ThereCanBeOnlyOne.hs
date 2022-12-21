module ThereCanBeOnlyOne where

onlyElem :: (Eq a) => a -> [a] -> Bool

-- idea check occurence, if found more than once return false
-- iterate over each element in a list, with a counter
-- if the element is found, increment the counter
-- if the counter is more than 1, return false immediatly
-- if the counter is 1, return true
-- if the list is empty, return false

onlyElem a [] = False
onlyElem a (x:xs) = onlyElemCounter a 0 (x:xs)

onlyElemCounter a b [] = if (b == 1) then True else False
onlyElemCounter a b (x:xs) = if (b == 2) then False
                             else if (a == x) then onlyElemCounter a (b+1) xs
                             else onlyElemCounter a b xs

-- example: onlyElem 5 [1,2,3,4,5] = True
-- example: onlyElem 5 [1,2,3,4,5,5] = False

onlyOnce :: (a -> Bool) -> [a] -> Bool
onlyOnce f [] = False
onlyOnce f (x:xs) = onlyOnceCounter f 0 (x:xs)

onlyOnceCounter f b [] = if (b == 1) then True else False
onlyOnceCounter f b (x:xs) = if (b == 2) then False
                             else if (f x) then onlyOnceCounter f (b+1) xs
                             else onlyOnceCounter f b xs

-- exact same concept but then generalized to function

-- onlyElem' should use onlyOnce
onlyElem' :: (Eq a) => a -> [a] -> Bool
onlyElem' a [] = False
onlyElem' a (x:xs) = onlyOnce (==a) (x:xs)
