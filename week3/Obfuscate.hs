module Obfuscate where

import Data.Char
import System.Random (getStdGen, randomRIO)
import Data.List (permutations)


--cambridge :: String -> String

meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."

transform :: String -> String
transform x
  | l <= 3     = x
  | otherwise  = head x : ((reverse (take (l-2) (tail x))) ++ [last x])
  where l = length x

cambridge s = unwords $ map transform $ words s
