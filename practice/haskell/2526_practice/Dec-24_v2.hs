{-
    see how the same recursive patterns from lists generalize to any structure
    you define.

    ---

    1. Anatomy of Algebraic Data Types
-}
import Data.Set (Set)
import Data.Set qualified as Set

import Data.List (sortOn)

import Data.Map (Map)
import Data.Map qualified as Map



data Bush a = Leaf a | Fork (Bush a) (Bush a)
data Tree a = Tip | Node (Tree a) a (Tree a)

{-
    Key distinction in your notes:
    - Bush: values at leaves (like berries)
    - Tree: values at nodes (like fruit), with empty `Tip`s at the ends

    Lists are just trees that only grow in one direction:
-}
-- data [a] = [] | a : [a]



-- 2. Recursion on Trees
    -- Same pattern as lists--base case(s) + recursive case(s)

sizeBush :: Bush a -> Int
sizeBush (Leaf _)     = 1
sizeBush (Fork lt rt) = 1 + sizeBush lt + sizeBush rt

depthBush :: Bush a -> Int
depthBush (Leaf _)     = 0
depthBush (Fork lt rt) = 1 + max (depthBush lt) (depthBush rt)

valuesBush :: Bush a -> [a]
valuesBush (Leaf x)     = [x]
valuesBush (Fork lt rt) = valuesBush lt ++ valuesBush rt

-- For `Tree`:
valuesTree :: Tree a -> [a]
valuesTree (Tip)          = []
valuesTree (Node lt x rt) = valuesTree lt ++ [x] ++ valuesTree rt

{-
    To make these O(N) instead of O(N^2), you need to use the Accumulator 
    Pattern (often implemented via a helper function, commonly named `go`).

    Instead of saying "Build Left, Build Right and Paste them together", you
    say: "Build Left and prepend it to the Result of building right."
-}

valuesBush' :: Bush a -> [a]
valuesBush' t = go t []
  where
    go :: Bush a -> [a] -> [a]
    go (Leaf x) acc     = x : acc
    go (Fork lt rt) acc = go lt (go rt acc)

{-
    Why is this faster?
    - `x : acc` is O(1).
    - We never need to traverse the list we've already built. We just keep 
      gluing new items onto the front.
-}



-- data Tree a = Tip | Node (Tree a) a (Tree a)
valuesTree' :: Tree a -> [a]
valuesTree' t = go t []
  where
    go :: Tree a -> [a] -> [a]
    go (Node Tip x Tip) acc = x : acc
    go (Node lt x rt) acc   = go lt (x : (go rt acc))  

{-
    How to read `go lt (x : go rt acc)`:
    1. `go rt acc`: Process the Right branch and glue it onto `acc`.
    2. `x : ...`: Stick `x` on front of that.
    3. `go lt ...`: Process the Left branch and glue it onto the front of 
        everything else.
-}


------ ------           ------ ------  ------   ------     ------ ------ ------
-- data Tree a = Tip | Node (Tree a) a (Tree a)
------ ------           ------ ------  ------   ------     ------ ------ ------

-- 3. Binary Search Trees
{-
    Convention: smaller values left, larger values right. This gives you 
    O(log n) search on balanced trees:
-}
member :: Ord a => a -> Tree a -> Bool
member x Tip            = False
member x (Node lt a rt)
  | x == a    = True
  | x > a     = member x rt
  | otherwise = member x rt

member' :: Ord a => a -> Tree a -> Bool
member' x Tip = False
member' x (Node lt y rt) = case compare x y of
  LT -> member' x lt 
  EQ -> True
  GT -> member' x rt

insert :: Ord a => a -> Tree a -> Tree a
insert x Tip = Node Tip x Tip 
insert x (Node lt y rt) = case compare x y of
  LT -> Node (insert x lt) y rt
  EQ -> Node lt y rt
  GT -> Node lt y (insert x rt)


-- Building a tree form a list--it's a fold!
grow :: Ord a => [a] -> Tree a
grow = foldr insert Tip

tsort :: Ord a => [a] -> [a]
tsort = valuesTree . grow



------ ------           ------ ------  ------   ------     ------ ------ ------

-- 4. Rose Trees (arbitrary branching)
data Rose a = Flower a | Vine [Rose a]

valuesRose :: Rose a -> [a]
valuesRose (Flower x) = [x]
valuesRose (Vine xs)  = concatMap valuesRose xs

-- Your L-Systems `Command` type from PPTs is a rose tree.


-- 5. Expression Trees

-- Trees don't just hold values--they encode structure:
data Expr = Val Int
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Neg Expr

{-
    `Mul (Val 5) (Add (Val 5) (Var "x"))` represents `5 * (5 + x)`. The 
    structure is the bracketing.
-}          


-- 6. Mapping over Trees

-- Same concept as `map` for lists--transform values, preserve structure:
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node Tip x Tip) = Node Tip (f x) Tip
treeMap f (Node lt x rt) = Node (treeMap f lt) (f x) (treeMap f rt)

{-
    Key exam insight: When you define a new datatype, the recursive structure of
    your functions mirrors the recursive structure of the type. One case per
    constructor, recurse on recursive fields.
-}

{-
    This insight is the "Golden Rule" of Functional Programming. It means you
    don't have to be "clever" to write a function; you just have to blindly
    follow the shape of the data.

    1. "One case per constructor"

    If your data type is defined with `|` (OR), your function MUST handle every
    single option using pattern matching.
-}

-- Example: A `Shape` Type
data Shape 
    = Circle Float          -- Constructor 1
    | Rectangle Float Float -- Constructor 2
    | Point                 -- Constructor 3

-- Since there are 3 constructors, your function needs exactly 3 equations:
area :: Shape -> Float
area (Circle r)      = undefined    -- Equation 1
area (Rectangle w h) = undefined    -- Equation 2
area Point           = undefined    -- Equation 3

-- If you miss one, the compiler warns you (Pattern match(es) are 
-- non-exhaustive.)


------ ------           ------ ------  ------   ------     ------ ------ ------

-- 2. "Recurse on recursive fields"

-- If the data definition refers to itself (recursion), your function must call
-- itself at that exact spot.

-- Example: A Simple List
        -- data [a] = [] | a : [a]
        --                     ^^^
        --              Recursive field here!

-- The Function Mirroring:        
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
--                  ^^^^^^^^^
--           Function calls itself here!


-- The "Fill-in-the-Blanks" Strategy

-- This insight makes writing code almost mechanical

{-
    Task: Write a function `sumTree` for this type:

```Haskell
data Tree = Leaf Int | Node tree Tree
```

    Step 1: Write the skeleton (One case per constructor)

```Haskell
sumTree (Leaf x)     = ...
sumtree (Node lt rt) = ...
```    

    Step 2: Identify recursion (Recurse on recursive fields)
    - `Leaf` has no `Tree` inside. No recursion.
    - `Node` has two `Tree`s inside (`lt` and `rt`). We must call `sumTree l`
      and `sumTree r`.

```Haskell
sumTree (Leaf x)   = x
sumTree (Node l r) = (sumTree l) + (sumTree r)
```

    Why is this an "Exam Insight?" In an exam, if you are stuck, write out the 
    cases for the constructors immediately. ...
-}

------ ------           ------ ------  ------   ------     ------ ------ ------
-- data Tree a = Tip | Node (Tree a) a (Tree a)
------ ------           ------ ------  ------   ------     ------ ------ ------

{-

    The mental model for almost all recursive data types in functional 
    programming.

    Basically, any data type that references itself is a Tree.

    1. Visualizing your Expr as a Tree
    
    Your `Expr` type is a perfect example of a tree structure, often called an
    Abstract Syntax Tree (AST).
    - The Leaves (Terminal Nodes): These are the end of the line. They don't
      contain any more `Expr`s inside them.
      - `Val Int` is a leaf.
      - `Val String` is a leaf.
    - The Branches (Non-Terminal Nodes): These connect to other nodes (which 
      might be branches or leaves).
      - `Neg Expr` is a branch with 1 child.
      - `Add Expr Expr` is a branch with 2 children.
      - `Mul Expr Expr`...

    If you wrote `Mul (Val 2) (Add (Val 3) (Val 4))`, it isn't just a flat line 
    of code; it is a 2D structure:


    2. Is everything a tree?

    Actually, yes. In Haskell, almost every standard data structure is a 
    variation of a tree:
    - Lists (`[a]`): A "lop-sides" tree that only ever branches to the right.
      - `1 : (2 : (3 : []))` is just a tree where every node has a value and one
        child, until you hit the empty leaf `[]`.
    - Maybe (`Maybe a`): A tiny tree with a max depth of 1.
      - `Nothing` is an empty leaf.
      - `Just x` is a node with one value and no children.

    3. Why this matters for your exam

    Because `Expr` is a tree, any function you write for it involves Tree
    Traversal.

    When you write `eval`, you are just writing a standard Depth-First Search 
    (DFS) algorithm, but the pattern matching hides the scary "algorithm" ...


        -- This is literally a tree traversal algorithm
        eval (Add e1 e2) = eval e1 + eval e2
        --                 ^^^^^^^   ^^^^^^^
        --                 Visit Left + Visit Right
-}



------ ------           ------ ------  ------   ------     ------ ------ ------

-- covers practical data structures from the `containers` library--Sets and 
-- Maps. This is where theory meets real-world efficiencies.

{-
    `newtype` is definitely more like `data`. It has nothing to do with `var` or
    `val` (which are about assigning values); `newtype` is about defining Types.

    Think of `newtype` as a Zero-Cost Wrapper.

    1. The Short Answer

      It creates a brand new type that is distinct from the original, but erased
      at runtime.
      - Compile Time: It is treated as a totally unique type (strict safety).
      - Run Time: It is identical to the underlying type (fast, no overhead).

    2. The Difference vs `Data`

      - `data`
        - Purpose: Create complex structures (trees, lists)
        - Runtime Cost: High. Wraps the value in a "box" (header + pointer)
        - Restriction: Can have many constructors and fields.

      - `newtype`
        - Purpose: Create a distinct name for an existing type.
        - Runtime Cost: Zero. The "box" dissapears. It is just the raw value.
        - Restriction: Must have exactly 1 constructor and 1 field.

    3. Example: The "Mars Climate Orbiter" Problem

      Imagine you have a rocket function that takes `Meters`.

      ```Haskell
      -- Using type (Aliases) - DANGEROUS
      type Meters = Double
      type Yards  = Double

      flyToMars :: Meters -> IO ()
      ```

      Because `Meters` and `Yards` are just aliases for `Double`, Haskell will 
      allow you to accidentally pass `Yards` into `flyToMars`. BOOM, crash.

      
      Using `data` (Safe, but slow):

      ```Haskell
      data Meters = Meters Double
      ```

      This is safe, but every time you use it, your program has to "unwrap" the
      box to get the number inside. That costs CPU cycles.

      Using `newtype` (Safe AND Fast):

      ```Haskell
      newtype Meters = Meters Double
      newtype Yards  = Yards Double
      ```

      - Safety: If you try to pass `Yards` to a function expecting `Meters`, the
        compiler throws an error.
      - Speed: Once compiled, the `Meters` wrapped is deleted. The machine code
        just sees a raw `Double`.

    Use `newType` when you want the Type Safety of a custom class, but the 
    Performance of a primitive `Int`, `Double` or `String`.
-}



{-
    1. Newtypes -- Zero-cost Type Distinctions

    `newtype` creates a new type with the same runtime representation but 
    different compile-time identity:
-}
newtype Rank = Rank Int
newtype File = File Int

-- Now you can't accidentally swap rank and file in chess coordinates. Same
-- runtime cost as `Int`, but the compiler catches mistakes.


-- Use Case: Multiple typeclass instances
newtype Sum = Sum Int
newtype Product = Product Int

-- Now Sum and Product can have different Monoid instances
-- even though they're both "just Ints" at runtime.

{-
                This touches on a fundamental restriction in Haskell: **A type can only have ONE instance for a given Typeclass.**

                For `Int`, this is a problem. Why? Because integers have **two** valid ways to behave like a `Monoid`:

                1. **Addition:** You combine them with `+` and the "empty" value is `0`.
                2. **Multiplication:** You combine them with `*` and the "empty" value is `1`.

                ### The Problem: Ambiguity

                If you tried to write this in Haskell, the compiler would panic:

                ```haskell
                -- HYPOTHETICAL / ILLEGAL CODE
                instance Monoid Int where
                    mempty = 0
                    mappend x y = x + y

                instance Monoid Int where
                    mempty = 1
                    mappend x y = x * y

                ```

                If you later wrote `3 <> 4`, Haskell wouldn't know if you meant `7` (Addition) or `12` (Multiplication).

                ### The Solution: `newtype` Wrappers

                Since we can't have two instances for `Int`, we create two new types that *contain* an `Int` but are treated as distinct distinct citizens by the compiler.

                #### 1. The `Sum` Wrapper

                When you wrap a number in `Sum`, you are telling Haskell: "Treat this `Int` as something that gets **added**."

                ```haskell
                newtype Sum a = Sum a

                instance Num a => Monoid (Sum a) where
                    mempty = Sum 0              -- Identity is 0
                    mappend (Sum x) (Sum y) = Sum (x + y) -- Operation is +

                ```

                #### 2. The `Product` Wrapper

                When you wrap a number in `Product`, you are telling Haskell: "Treat this `Int` as something that gets **multiplied**."

                ```haskell
                newtype Product a = Product a

                instance Num a => Monoid (Product a) where
                    mempty = Product 1              -- Identity is 1
                    mappend (Product x) (Product y) = Product (x * y) -- Operation is *

                ```

                ### In Practice

                This allows you to use generic functions (like `fold` or `mconcat`) that work on *any* Monoid, and simply swap the wrapper to change the behavior.

                ```haskell
                numbers = [2, 3, 4]

                -- "I want to add them"
                getSum (mconcat (map Sum numbers))
                -- Logic: 0 + 2 + 3 + 4
                -- Result: 9

                -- "I want to multiply them"
                getProduct (mconcat (map Product numbers))
                -- Logic: 1 * 2 * 3 * 4
                -- Result: 24

                ```

                ### Summary

                * **Without `newtype`:** `Int` has an identity crisis (Add? Multiply?).
                * **With `newtype`:** `Sum` and `Product` are like assigning a specific job or role to that `Int`, allowing the compiler to pick the correct logic (`+` vs `*`) with zero performance cost.
-}


-- Use Case: Altering behavior
newtype Down a = Down a deriving (Eq)
instance Ord a => Ord (Down a) where
    Down x <= Down y = x >= y

ex = sortOn Down [3, 1, 2]     -- [3, 2, 1], reverse sort in one pass


------ ------           ------ ------  ------   ------     ------ ------ ------

-- 2. Sets -- Balanced BSTs with No Duplicates
-- import Data.Set (Set)
-- import Data.Set qualified as Set

xs = Set.fromList [1, 2, 3]
ys = Set.fromList [3, 4, 5]
a = Set.union xs ys     -- {1, 2, 3, 4, 5}
b = Set.member 3 xs     -- True
c = Set.insert 4 xs     -- {1, 2, 3, 4}

{-
    All operations are O(log n) because the underlying tree is balanced (unlike
    your hand-rolled `grow`).
-}

-- Practical example: Better `nub`

-- The standard `nub :: Eq a => [a] -> [a]` is O(n^2). With `Ord`, we can do
-- O(n log n):

{-
    - `Data.List.nub`: Removes duplicates but keeps the original order.
-}

nubOrd :: Ord a => [a] -> [a]
nubOrd xs = nub' Set.empty xs
  where
    nub' :: Ord a => Set a -> [a] -> [a]
    nub' _ [] = []
    nub' seen (x:xs)
      | Set.member x seen = nub' seen xs
      | otherwise         = x : nub' (Set.insert x seen) xs

{-
    Note: `Set.toList . Set.fromList` removes duplicates but also sorts--
    sometimes you need to preserve order.
-}



------ ------           ------ ------  ------   ------     ------ ------ ------

-- 3. Maps -- Key-Value Balanced Trees

-- import Data.Map (Map)
-- import Data.Map qualified as Map

m = Map.fromList [("x", 0), ("y", 1)]
ma = Map.lookup "x" m        -- Just 0
mb = m Map.! "x"             -- 0 (partial, crashes if missing)
mc = Map.insert "z" 2 m      -- adds new entry

{-
    Key functions:
    - `Map.member :: Ord k => k -> Map k v -> Bool`
    - `Map.insert :: Ord k => k -> v -> Map k v -> Map k v`
    - `Map.adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v`
    - `Map.insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v`
-}



-- 4. Building a `groupBy` Function

-- Group items by a key function:
groupBy' :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy' f = foldr (\x -> Map.insertWith (++) (f x) [x]) Map.empty

{-
    `insertWith (++) k [x]` says: "insert `[x]` at key `k`, but if the key 
    exists, prepend `[x]` to the existing line using `(++)`."

```Haskell
groupBy colour [Apple, Pear, Lemon, Banana]
-- Map.fromList [(Green, [Apple, Pear]), (Yellow, [Lemon, Banana])]
```


    Exam relevance...
    1. Balanced trees give O(log n) operations
    2. Newtypes are zero-cost type safety
    3. The same fold patterns apply to building Maps. <-- applies to list and sets too it seems... irrelevant for now and i will skip ahead for now

    ... will help you reason about efficiency and design.
-}


{-
    Think of `class` as a Club Definition and an `instance` as a Membership
    Application.

    The Club Definition (class)
    This defines the rules. "To be in the Eq club, you must define (==)."
-}
class Eq a where
  (==) :: a -> a -> Bool

{-
    The Membership Application (instance)
    This explains how a specific type follows those rules.
-}


{-
    4. What was the lecturer trying to teach?

    Beyond the syntax, there were three hidden lessons:

    1. Constraints Propagate: If you want to compare a list `[a]`, you must know
       how to compare the elements [a]. This appears in the type signature: 
       `instance Eq a => Eq [a]`.
    2. Deriving is Powerful: GHC is smart. Using `deriving (Eq, Ord, Show)` 
       saves you from writing boilerplate code that handles every constructor.
    3. Laws Matter: Typeclasses come with "laws" (e.g., `x == y` means `y == x`)
       . The compiler does not check these, but if you break these, your code
        will behave unpredictably.

    
    5. Cheat Sheet
       - Constraints: Always appear before the `=>` (e.g. `Eq a => ...`).
       - Instance Heads: Can have their own constraints. 
         `instance Eq a => Eq [a]` means "List of `a` is comparable IF `a` is
         comparable".
       - Deriving: Always derive `Eq, Ord, Show, Read` when possible.
       
         
    -}
------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------


class Show a where
  show :: a -> String

class Read a where
  read :: String -> a  

-- Law: `read . show = id`

{-
C. Show and Read (String Conversion)
Show: Converts data to a String (show :: a -> String). Required for GHCi to print results.

Read: Converts a String to data (read :: String -> a).

The Lesson: Never write these by hand. Hand-written parsers are error-prone. Always derive them.

D. Num (Custom Numbers)
This was the "hard" part of the lecture.

Goal: Make your custom Nat type support 5 + 3.

How: You define an instance of Num Nat.

fromInteger: This is the magic function that turns the literal 5 into your custom S (S (S (S (S Z)))).

Recursion: You implemented (+) and (*) by teaching Haskell how to add/multiply your custom structure recursively.
------ ------           ------ ------  ------   ------     ------ ------ ------