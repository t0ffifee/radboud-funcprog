import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--  ^^comment out this line if it causes compile errors (we need the `containers` library)

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

-- example: wordFrequency "this is a test of the emergency broadcast system"

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength n = filter ((>=n) . length) . map head . group . sort . words

wordLengthFrequency :: String -> [(Int,Int)]
wordLengthFrequency = map (\x->(head x,length x)) . group . sort . map length . words

anagrams :: String -> [[String]]
anagrams = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map (\x->(sort x,x)) . words

{- this 'main' function is only here for the compiled, stand-alone version
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats
 -
 - (The -O flag turns on optimizations)
 -}
main :: IO ()
main = onStdin $ wordFrequency  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))
