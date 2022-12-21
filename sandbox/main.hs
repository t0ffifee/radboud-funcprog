swap :: (Int, Int) -> (Int, Int)
swap (a,b) = (b, a)

group :: (Int, (Char, Bool)) -> (Int, Char, Bool)
group (i, (c, b)) = (i, c, b)

foo x y
    | x == "" = y
    | otherwise = x

bar b x y = if b then (x,y) else (y,x)

foobar  x = \y -> x

foobla = ("Haskell" ++)