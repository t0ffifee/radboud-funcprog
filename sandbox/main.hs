import Control.Parallel
import Data.List
import Data.Char
import Data.Maybe

swap :: (Int, Int) -> (Int, Int)
swap (a,b) = (b, a)

group :: (Int, (Char, Bool)) -> (Int, Char, Bool)
group (i, (c, b)) = (i, c, b)

foo x y
    | x == "" = y
    | otherwise = x


-- takes a list of integers and returns the sum of the even integers in the list
sumEven xs = sum $ filter (\x -> mod x 2 == 0) xs

containsChar :: Char -> String -> Bool
containsChar c s = elem c s

-- takes a list of strings and returns a list of the strings that contain the letter 'a'
onlyAStrings :: [String] -> [String]
onlyAStrings xs = filter (\x -> containsChar 'a' x) xs

-- takes a list of strings and returns a list of the strings that are palindromes
palindromes :: [String] -> [String]
palindromes xs = filter (\x -> x == reverse x) xs

-- function that takes a list of strings and returns a list of the strings that are anagrams of each other

-- areAnagrams :: String -> String -> Bool
-- areAnagrams (a,b) = sort a == sort b

-- findAnagrams :: [String] -> [String]
-- findAnagrams [] = []
-- findAnagrams (x:xs) = areAnagrams x



-- Function to check if two strings are anagrams
isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

-- Function to return a list of anagrams from a list of strings
findAnagrams :: [String] -> [String]
findAnagrams [] = []  -- Base case: return an empty list if the input list is empty
findAnagrams (x:xs) =  -- Recursive case: check x against the rest of the list
  x : filter (isAnagram x) xs ++ findAnagrams (filter (not . isAnagram x) xs)

main = do
  let list = ["anagram", "listen", "silent", "hello", "world"]
  let anagrams = findAnagrams list
  print anagrams

-- parallel quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let smallerSorted = quicksort (filter (<=x) xs)
                       biggerSorted  = quicksort (filter (>x) xs)
                   in smallerSorted `par` (biggerSorted `pseq` (smallerSorted ++ [x] ++ biggerSorted))

ys = ["anagram", "listen", "silent", "hello", "world"]
xs = [12,43,56,72,95,10,32,64,87,21,33,98,90,89,786]

pop :: [n] -> (n, [n])
pop xs = let x = head $ reverse xs
             xs' = reverse $ tail $ reverse xs
         in (x, xs')

push :: [n] -> n -> [n]
push xs x = xs ++ [x]

top :: [n] -> n
top xs = head $ reverse xs

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 n
  | n < 0 = error "illegal input"
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

(~~) :: String -> String -> Bool 
xs ~~ ys = map toLower xs == map toLower ys

reverseCase :: String -> String
reverseCase [] = []
reverseCase (x:xs)
    | isLower x = toUpper x : reverseCase xs 
    | otherwise = toLower x : reverseCase xs

betterReverseCase :: String -> String
betterReverseCase s = map (\c -> if isUpper c then toLower c else toUpper c) s

shift :: Int -> Char -> Char
shift n x
    | isAscii x && isUpper x = chr ( ord 'A' + (ord x - ord 'A' + n) `mod` 26 )
    | otherwise = x 

caesar :: Int -> String -> String
caesar n xs = map (\x -> shift n (toUpper x)) xs

message = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"