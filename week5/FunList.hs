module FunList where

--define using the _list design pattern_
compose :: [a -> a] -> (a -> a)
compose [] x = x
compose (f:fs) x = f (compose fs x)

--define using `foldr`
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

--Explain _what_ the following function computes, and _how_ it computes it
{-

 Your explanation here

-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

-- foo 5 = compose (map (*) [1..5]) 1
--      = compose [(*1), (*2), (*3), (*4), (*5)] 1
--      = compose [(*1), (*2), (*3), (*4), (*5)] 1
--      = (*1) (compose [(*2), (*3), (*4), (*5)] 1)
--      = (*1) ((*2) (compose [(*3), (*4), (*5)] 1))
--      = (*1) ((*2) ((*3) (compose [(*4), (*5)] 1)))
--      = (*1) ((*2) ((*3) ((*4) (compose [(*5)] 1))))
--      = (*1) ((*2) ((*3) ((*4) ((*5) (compose [] 1)))))
--      = (*1) ((*2) ((*3) ((*4) ((*5) 1))))
--      = (*1) ((*2) ((*3) ((*4) 5)))
--      = (*1) ((*2) ((*3) 20))
--      = (*1) ((*2) 60)
--      = (*1) 120	
--      = 120

-- it calculates the permutation x! so 5! = 5 * 4 * 3 * 2 * 1 in that order


--define in terms of *only* `map` and `compose`
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
-- use map and compose
foldr' f z (x:xs) = compose (map f (x:xs)) z

