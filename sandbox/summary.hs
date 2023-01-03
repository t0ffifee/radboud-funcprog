

-- takes two lists and returns True iff the first list is a prefix of the second
isPrefixOf :: Eq a => [a] -> [a] -> Bool

-- takes two lists and returns True iff the first list is a suffix of the second 
isSuffixOf :: Eq a => [a] -> [a] -> Bool

-- takes two lists and returns True iff the first list is contained, wholly and intact, anywhere within the second
isInfixOf :: Eq a => [a] -> [a] -> Bool

-- takes two lists and returns True if all the elements of the first list occur, in order, in the second
-- the elements do not have to occut consecutively
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool

fst
snd
words
interact
read
show
foldl
foldr
concat
tails
head
tail
:
++
or
and
map
sort
sortBy
length
zip
compare
reverse
isUpper
isLower
isAscii
toLower
toUpper
fromIntegral etc.
filter
not
sum
take
drop
dropWhile
takeWhile
group