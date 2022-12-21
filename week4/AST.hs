module AST where

-- 4.6.1
data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Div Expr Expr | VarX

-- 4.6.2
eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit x) _ = Just (fromInteger x)
eval (Add x y) z = (+) <$> eval x z <*> eval y z
eval (Mul x y) z = (*) <$> eval x z <*> eval y z
eval (Sub x y) z = (-) <$> eval x z <*> eval y z
eval (Div x y) z 
  | eval y z == Just 0 = Nothing
  | otherwise = (/) <$> eval x z <*> eval y z
eval (VarX) z = Just z


--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
