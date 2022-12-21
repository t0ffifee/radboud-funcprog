module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

{----------- exercise 4.3 -------------}

-- 4.3.1
-- Node 'c' (Node 'a'  (Leaf) (Node 'b' Leaf Leaf)) (Node 'f' (Node 'd' Leaf Leaf ) (Node 'g' Leaf Leaf))

-- is the above tree a binary search tree?
-- yes, because the left subtree of every node is smaller than the node and the right subtree is bigger than the node

-- 4.3.2
--                3
--            2       4
--          1   L   5    L
--         L  L    L L    

-- 4.3.3
-- TODO: check this on actual tree

leaves :: Tree a -> Int 
leaves Leaf = 1
leaves (Node _ l r) = leaves l + leaves r

nodes :: Tree a -> Int
nodes Leaf = 0
nodes (Node _ l r) = 1 + nodes l + nodes r

height :: Tree a -> Int
height Leaf = 0
height (Node _ l r) = 1 + max (height l) (height r)

elems :: Tree a -> [a]
elems Leaf = []
elems (Node x l r) = x : elems l ++ elems r

-- binary search tree: the left subtree of every node is smaller than the node and the right subtree is bigger than the node
-- for each element check that l is smaller and r is bigger so l is leaf or smaller than x and r is leaf or bigger than x

isLeaf :: Tree a -> Bool
isLeaf Leaf = True
isLeaf _ = False

minElem :: Tree a -> a
minElem (Node x Leaf _) = x
minElem (Node _ l _) = minElem l

maxElem :: Tree a -> a
maxElem (Node x _ Leaf) = x
maxElem (Node _ _ r) = maxElem r

isBST :: Ord a => Tree a -> Bool
isBST Leaf = True
isBST (Node x l r) = isBST l && isBST r && (isLeaf l || x > maxElem l) && (isLeaf r || x < minElem r)

-- 4.3.4

member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member x (Node y l r) = x == y || member x l || member x r

-- use member to make sure that the element is not already in the tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y l r) = if x == y then Node y l r else if x < y then Node y (insert x l) r else Node y l (insert x r)

fromlist :: Ord a => [a] -> Tree a
fromlist [] = Leaf
fromlist (x:xs) = insert x (fromlist xs)

-- delete, find the node with the element, then delete it
-- we delete it by replacing it with leaf and updating possible children
-- if the node has no children, we just replace it with leaf
-- if the node has one child, we replace it with the child
-- if the node has two children, we replace it with the smallest element in the right subtree
-- we can do this because the right subtree is bigger than the node, so the smallest element is the smallest element in the right subtree
-- we can also do this because the smallest element in the right subtree has no left child, so we can replace it with the right child
delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Node y l r) = if x == y then case (l, r) of
                                            (Leaf, Leaf) -> Leaf
                                            (Leaf, _) -> r
                                            (_, Leaf) -> l
                                            (_, _) -> Node (minElem r) l (delete (minElem r) r)
                  else if x < y then Node y (delete x l) r else Node y l (delete x r)

{----------- exercise 4.5 -------------}

-- the idea is that the right bottom element should always be the smallest and the left bottom the biggest
-- so we start from the right bottom, work our way to the root and then traverse the left bottom part
inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node x l r) = inOrder r ++ [x] ++ inOrder l

-- creates a balanced binary trees from a list sorted in ascending order
-- start with the root node, then the left and right subtrees are the left and right halves of the list
-- then we do the same for the left and right subtrees
-- we can do this because the list is sorted in ascending order
-- the left subtree will always be smaller than the root and the right subtree will always be bigger than the root
-- so we can split the list in two halves and the left half will be the left subtree and the right half will be the right subtree
fromAscList :: [a] -> Tree a
fromAscList [] = Leaf
fromAscList xs = Node (xs !! (length xs `div` 2)) (fromAscList (take (length xs `div` 2) xs)) (fromAscList (drop (length xs `div` 2 + 1) xs))

-- idea: first add the value of root, then the value of the first element in the left subtree, then the value of the first element in the right subtree,
-- after getting the value of the left child, we should get the value of the right child before going deeper in our recursive call, only after that should we 
-- continue recursively

breadthFirstHelper :: Tree a -> [a]
breadthFirstHelper Leaf = []
breadthFirstHelper (Node x l r) = evalonechild l ++ evalonechild r ++ breadthFirstHelper l ++ breadthFirstHelper r
  where evalonechild Leaf = []
        evalonechild (Node x _ _) = [x]

breadthFirst :: Tree a -> [a]
breadthFirst Leaf = []
breadthFirst (Node x l r) = [x] ++ evalonechild l ++ evalonechild r ++ breadthFirstHelper l ++ breadthFirstHelper r
  where evalonechild Leaf = []
        evalonechild (Node x _ _) = [x]

{----------- exercise 4.6 -------------}
--breadthFirst :: Tree a -> [a]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}

{-
layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "┤\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "╭─"  -- change to "/-" if no Unicode
  v_bar = fill ++ "│ "  -- change to "| " if no Unicode
  lbend = fill ++ "╰─"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
-}
