module AnyAll where

-- we define All' using Any, not and .
All' :: (a -> Bool) -> [a] -> Bool
All' p = not . Any (not . p)