module Triangle where

stars :: Int -> String
stars n = replicate n '*'

spaces :: Int -> String
spaces n = replicate n ' '

line :: Int -> Int -> String
line base numberOfStars = spaces numberOfSpaces ++ stars numberOfStars ++ "\n"
  where numberOfSpaces = (base - numberOfStars) `div` 2

nums :: Int -> [(Int, Int)]
nums n = zip (replicate n n)  ([n, n-2..1])

convert :: Int -> Int
convert n = 1 + 2 * n -2

triangle :: Int -> String
triangle n = concat $ reverse $ map (\(a, b) -> line a b) $ nums $ convert n
