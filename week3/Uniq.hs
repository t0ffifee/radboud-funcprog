module Uniq where

-- removes duplicate contiguous items in a list, for example:
-- example: uniq "goodbye, hello" ⟹ "godbye, helo"

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) = if x == y then uniq (y:xs) else x : uniq (y:xs)