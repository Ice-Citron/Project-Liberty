test_rotate, test_flipV, test_flipH :: Bool

test_rotate = flipV (flipH horse) == horse
test_flipV  = flipV (flipV horse) == horse
test_flipH  = flipH (flipH horse) == horse

--------------------------------------------------------------------------------

prop_rotate, prop_flipV, prop_flipH :: Picture -> Bool

prop_rotate pic = flipV (flipH horse) == horse
prop_flipV  pic = flipV (flipV horse) == horse
prop_flipH  pic = flipH (flipH horse) == horse

> quickCheck prop_rotate
> quickCheck prop_flipV
> quickCheck prop_flipH

--------------------------------------------------------------------------------

-- filter keeps only elements that satisfy a condition (predicate)
filter :: (a -> Bool) -> [a] -> [a]

-- Examples
filter even [1, 2, 3, 4, 5]     -- [2, 4]
filter (>3) [1, 2, 3, 4, 5]     -- [4, 5]
filter (/= 'a') "banana"        -- "bnn"
filter null [[1], [], [2, 3]]   -- [[]]

-- It applies the function to each element, keeps the one where it returns
-- *True*, discards where it returns False.

--------------------------------------------------------------------------------

-- '(!)' is the lookup operator for 'Data.Map' - it gets the value for a key.

(!) :: Ord k => Map k v -> v

-- Example
import qualified Data.Map as M

myMap = M.fromList [(1, "one"), (2, "two"), (3, "three")]

myMap ! 1       -- "one"
myMap ! 2       -- "two"
myMap ! 99      -- ERROR! Key not found (crashes)

-- Key differences from `lookup`:
M.lookup :: Ord k => k -> Map k v -> Maybe v
-- Safe, returns Nothing if key doesn't exist

(!) :: Ord k => Map k v -> k -> v
-- Unsafem crashes if key doesn't exist

M.lookup 1 myMap    -- Just "one"
M.lookup 99 myMap   -- Nothing (safe)

myMap ! 1           -- "one"
myMap ! 99          -- ERROR: key not found (crash)

-- Bottom line: use `(!)` when you're sure the key exists, otherwise use
--              `M.lookup` for safety!

--------------------------------------------------------------------------------

-- `($)` is just function application with the lowest precedence.

($) :: (a -> b) -> a -> b
f $ x = f x

-- Why use it?

-- To avoid parentheses!

-- Without $:
sum (filter even (map (*2) [1, 2, 3]))

-- With $:
sum $ filter even $ map (*2) [1, 2, 3]

-- It applies the function on the left to everythinbg on the right.


-- Key difference:
f (g x)     -- normal application (high precedence)
f $ g x     -- same thing (low precedence, right-assosciative)

-- More examples:
print (5 + 3)       -- 8
print $ 5 + 3       -- 8 (cleaner!)

length (take 5 [1..])
length $ take 5 [1..]

head (reverse ( filter odd [1, 2, 3, 4, 5]))
head $ reverse $ filter odd [1, 2, 3, 4, 5]

-- Bottom line: Use `$` to avoid writing closing parentheses at the end of
-- expressions. It's purely stylistic!

--------------------------------------------------------------------------------

-- Complete Guide to Trees in Haskell

-- 1. Basic Binary Tree Definition

-- Simple Binary Tree:
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- This defines:
-- - `Empty` - an empty tree (leaf)
-- - `Node value leftSubtree rightSubtree` - a node with a value and two
--                                           children

-- Examples:
-- Single node
tree1 :: Tree Int
tree1 = Node5 Empty Empty

-- Small tree:
--     10
--    /  \
--   5    15
tree2 :: Tree Int
tree2 = Node 10
        (Node 5 Empty Empty)
        (Node 15 Empty Empty)

-- Bigger tree:
--       10
--      /  \
--     5    15
--    / \     \
--   3   7    20
tree3 :: Tree Int
tree3 = Node 10
        (Node 5 
            (Node 3 Empty Empty) 
            (Node 7 Empty Empty))
        (Node 15 
            Empty 
            (Node 20 Empty Empty))


-- 2. Basic Tree Operations

-- A. Size - Count all nodes
size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + (size left) + (size right)

> size tree3
6

-- B. Depth/Height - Maximum depth
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ left right) = 1 + (max (depth left) (depth right))

> depth tree3
3

-- C. Sum - Sum all valyes (for numeric trees)
sumTree :: Num a => Tree a -> a
sumTree Empty = 0
sumTree (Node x left right) = x + (sumTree left) + (sumTree right)

-- D. Flatten to List (In-order traversal)
flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node x left right) = flatten left ++ [x] ++ flatten right

-- E. Map 
mapTree :: (a  -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)


-- 3. Tree Traversals

-- A. Pre-order (Root, Left, Right)
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x left right) = [x] ++ preOrder left ++ preOrder right

-- B. In-order (Left, Root, Right)
inOrder :: Tree a -> a
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] inOrder right

-- C. Post-order (Left, Right, Root)
postOrder :: Tree a -> a
postOrder Empty = []
postOrder (Node x left right) = postOrder left ++ postOrder right ++ [x]

-- D. Level-order (Breadth-first)
import Data.List (tranpose)

levelOrder :: Tree a -> [a]
levelOrder tree = concat (levels tree)

levels :: Tree a -> [[a]]
levels Empty = []
levels (Node x left right) = [x] : merge (levels left) (levels right)
  where
    merge :: [[a]] -> [[a]] -> [[a]]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = (x ++ y) : merge xs ys

-- D. Alternative BFS
bfs :: Tree a -> [a]
bfs tree = bfsHelper [tree]
  where
    bfsHelper :: [Tree a] -> a
    bfsHelper [] = []
    bfsHelper (Empty : rest) = bfsHelper rest
    bfsHelper (Node x left right : rest) =
        x : bfsHelper (rest ++ [left, right])


-- 4. Binary Search Tree (BST) Operations

-- A BST maintains the property: left < root < right

-- A. Insert into BST
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = x
insert x (Node y left right)
  | y < x     = Node left (insert x right)
  | x < y     = Node (insert x left) right
  | otherwise = Node left right     -- x == y, don't insert duplicates

> let bst = insert 10 $ insert 5 $ insert 15 $ insert 3 $ insert 20 Empty

-- B. Search in BST
search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search target (Node x left right)
  | target == x = True
  | x < target  = search target right
  | otherwise   = search target left        -- target < x

-- - Assumes tree is a BST (left < parent < right)
-- - Searches ONE branch only (O(log n) for balanced tree)
-- - Fast but only works on BSTs

-- B. Exhaustive Search (my implementation, safe, but inefficient)
search :: Ord a => a -> Tree a -> Bool
search _ Empty                    = False
search target (Node x left right) = 
    x == target || search target left || search target right

-- C. Find Minimum
finMin :: Tree a -> Maybe a
findMin Empty = Nothing
findMin (Node x Empty _) = Just x
findMin (Node _ left  _) = findMin left

-- D. Find Maximum
findMax :: Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node x _ Empty) = Just x
findMax (Node _ _ right) = findMax right

-- E. Delete from BST
delete :: Ord a => a -> Tree a -> Tree a
delete target Empty = Empty
delete target tree@(Node x left right)
  | target < x = Node x (delete x left) right
  | x < target = Node x left (delete x right)
  | otherwise = deleteNode tree
  where
    deleteNode (Node _ Empty right) = right
    deleteNode (Node _ left  Empty) = left
    deleteNode (Node _ left  right) =
      let Just minRight = findMin right 
      in  Node minRight left (delete minRight right)
      -- uses right branch's smallest value as new root... but need to delete
      -- minRight now as a consequence from the right branch 


-- 5. Folding Over Trees

-- A. FoldTree (like foldr for lists)
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ acc Empty               = acc
foldTree f acc (Node x left right) = 
    f x (foldTree f acc left) (foldTree f acc right)

-- Using `foldTree`
-- Size using fold
size' :: Tree a -> Int
size' = foldTree (\_ l r -> 1 + l + r) 0

-- Sum using fold
sumTree' :: Num a => Tree a -> a
sumTree' = foldTree (\x l r -> x + l + r) 0

-- Depth using fold
depth' :: Tree a -> Int
depth' = foldTree (\_ l r -> 1 + (max l r)) 0

--------------------

-- C. Expression Tree

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          deriving (Show, Eq)

-- Represents: (3 + 5) * (10 - 2)
expr :: Expr
expr = Mul (
         (Add (Val 3) (Val 5))
         (Sub (Val 10) (Val 2))
       )

-- Evaluation:
eval :: Expr -> Int
eval (Val n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2

-- Pretty printing:
showExpr :: Expr -> String
showExpr (Val n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ") "

--------------------------------------------------------------------------------

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

-- This means: `Tree a` will automatically implement `Show` and `Eq`, as long
-- as `a` also implements `Show` and `Eq`.

-- Examples

-- This works:
tree1 :: Tree Int
tree1 = Node (Leaf 1) (Leaf 2)

show tree1      -- Works! Int has `Show`
tree1 == tree1  -- Works! Int has Eq

-- This fails:
data NoShow = NoShow    -- No deriving Show

tree2 :: Tree NoShow
tree2 = Leaf NoShow

show tree2      -- ERROR! NoShow doesn't have Show


-- The constraints propagates like this:
-- When you derive Show for Tree:
instance Show a => Show (Tree a) where ...
--       ^^^^^^
-- This constraints: "a must be Show"

-- So the type becomes:
tree :: Show a => Tree a

-- Bottom line: `deriving (Show, Eq)` makes `Tree a` showable and comparable,
--              but only if `a` is also showable and comparable.

--------------------------------------------------------------------------------

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- "Write a function to check if a value exists in the tree"
checkVal :: a -> Tree a -> Bool
checkVal _ Empty = False
checkVal y (Node x left right) = y == x || checkVal y left || checkVal y right

-- "Write a function to get all leaves"
leaves :: Tree a -> [a]
leaves Empty                = []
leaves (Node x Empty Empty) = [x]
leaves (Node _ left  right) = leaves left ++ leaves right

--------------------------------------------------------------------------------

-- defintions of `foldl` and `foldr`
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []       = acc
foldl f acc s@(x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []       = acc
foldr f acc s@(x:xs) = f x (foldr f acc xs)