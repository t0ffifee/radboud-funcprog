module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol, toufik :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
toufik =  ("Toufik", 22,  "Procedural Programming")
george =  ("George", 22,  "Hacking in Haskell")

students :: [Person]
students = [elena, peter, pol, toufik, george]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_, _, n) = n

showPerson :: Person -> String
showPerson (a, b, c) = a ++ " "  ++ show b ++ " " ++ c

twins :: Person -> Person -> Bool
twins (_, a1, _) (_, a2, _) = a1 == a2
  
increaseAge :: Person -> Person
increaseAge (a, b, c) = (a, b+1, c)

-- first develop the expressions in GHCi, then replace the TODO's below with them
query1, query2, query3, query4, query5, query6 :: [Person]

query1 = let doubleAge p = (name p, age p + 2, favouriteCourse p) in map doubleAge students

query2 = let promote p = ("dr. " ++ name p, age p, favouriteCourse p) in map promote students

query3 = let fritsFinder p = name p == "Frits" in filter fritsFinder students

query4 = let inTwenties p = age p >= 20 in filter inTwenties students

query5 = realToFrac (sum (map age students)) / fromIntegral (length students)

query6 = map (\p -> if favouriteCourse p == "Functional Programming" then ("dr. " ++ name p, age p, favouriteCourse p) else p) students

