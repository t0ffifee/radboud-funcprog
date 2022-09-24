module ListComprehensions where

-- Polymorphic
alltuplecombinations :: [a] -> [b] -> [(a,b)]
alltuplecombinations as bs = [ (a,b) | a <- as, b <- bs ]


-- foreach a in as
-- foreach b in bs
-- add (a,b) to the list
-- so for example g0 [3,4] [1,2]
-- [(3,1),(3,2),(4,1),(4,2)]

-- Overloaded by type num
--nTimesY :: Num a => a -> [a]
nTimesY n y   = [ y | i <- [1..n] ]

-- foreach i in [1..n]  y (so y n times)
-- so for example g1 5 3 
-- [3,3,3,3,3]

-- requires integer/num values (no general polymorphism)
takeFirstN :: Int -> [a] -> [a]
takeFirstN n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

-- foreach (i,x) in zip [0..] xs where i < n
-- so for example g2 3 [1,2,3,4,5]
-- [1,2,3]

-- polymorphic
indexOf :: Eq a => a -> [a] -> [Int]
indexOf a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

-- get the index of a in xs

orderFirst xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- foreach (x,y) in zip xs ys 
-- add x and y to the list
-- so for example g4 [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]

-- fully polymorphic
getInner :: [[a]] -> [a]
getInner xss   = [ x | xs <- xss, x <- xs ]

-- foreach xs in xss
-- foreach x in xs
-- add x to the list
-- so we get the inside of a list as a list