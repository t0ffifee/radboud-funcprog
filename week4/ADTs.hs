module ADTs where

--EX 4.1
data Day       = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show
-- Possible expressions are "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"

data Prop      = Prop :-> Prop | T | F
  deriving Show
-- Possible expressions are "T", "F", "p :-> q", "p :-> (q :-> r)"

data Unit      = Unit
  deriving Show
-- Possible expressions are "Unit"

data Foo a     = Bar a
  deriving Show
-- Possible expressions are "Bar 3", "Bar True", "Bar "Hello""

data Tuple a b = Two a b | One a | None
  deriving Show
-- Possible expressions are "Two 3 True", "Two 3 False", "One 3", "None"

data Wrapped a = Wrapped (Wrapped a) | Bare a
  deriving Show
-- Possible expressions are "Bare 3", "Wrapped (Bare 3)", "Wrapped (Wrapped (Bare 3))", ...


--EX 4.2
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False


bval :: Prop -> Bool
bval prop = case prop of
              T         -> True
              F         -> False
              (p :-> q) -> not (bval p) || bval q

incr :: Foo [String] -> Foo String
incr (Bar k) = Bar (unlines k)

--wrap :: Integer -> Integer -> a -> Wrapped a
wrap 0 x = Bare x
wrap n x = wrap (n-1) (Wrapped x)
