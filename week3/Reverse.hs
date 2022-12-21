module Reverse where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev []     acc = acc
        rev (y:ys) acc = rev ys (y:acc)

-- I normally would prefer tail recursive functions but the nature of haskell makes it so that it does not matter that much anymore.
-- The reverse' actually uses less memory than the reverse function. This is because the reverse function has to build up the list
-- in the stack frame and then reverse it. The reverse' function does not have to do this because it is tail recursive.
-- It also ends with the last operation resulting in the list being reversed. Making it more suitable for loops etc
