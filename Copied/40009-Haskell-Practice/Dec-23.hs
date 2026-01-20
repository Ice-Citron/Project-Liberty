-- 1. Function definitions & pattern matching

{-
    - Functions have a type signature, then definitions
-}

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- The key insight: more specific patterns go first, and Haskell checks them
-- top-to-bottom. Guards let you add conditions.

fib n
  | n < 0     = undefined
  | otherwise = fib (n - 1) + fib (n - 2)


-- 2. Lazy evaluation (pass-by-need)
{-
    This is probably the most important conceptual piece. Haskell evaluates
    outermost-first and only when needed:
-}

two :: Integer -> Integer
two _ = 2

two infinity        -- returns 2, never evaluates infinity!

{-
    Contrast with eager languages where `two infinity` would loop forever trying
    to evaluate the argument first.

    Pattern matching forces evaluation though--`fact n` needs to know what `n`
    is to pick which equation to use.
-}


-- 3. Core types
{-
    - `Int`: bounded, 64-bit
    - `Integer`: unbounded
    - `Bool`
    - `Char`: uses single quotes
    - `String`: uses double quotes (and `String = [Char]`)
    - Tuples: fixed size, mixed types -- `(Int, Bool, Char)`
    - Lists: variable size, uniform types -- `[Int]`, built with `[]` and `(:)`
-}



------ ------           ------ ------  ------   ------     ------ ------ ------

-- 1. Lists -- structure and recursion

{-
    Lists are built from two primitives:
    - `[]` (nil/empty list)
    - `(:)` (cons, right-assosciative)

    So `[1, 2, 3]` is really 1 : (2 : ( 3 : []))
-}

-- The canonical recursive pattern on lists:
length :: [a] -> Int
length [] = 0                   -- base case
length _@(_:xs) = 1 + length xs -- recursive case

{-
    This shape appears everywhere: `sum`, `concat`, `++`. 

     - `sum`: adds up all the numbers in a list (or other foldable structures)
     - `concat`: takes a list of lists and flattens them into a single list. It
       essentially removes one layer of nesting.
     - `++` (Append operator): This is an infix operator that joins two specific
       lists together end-to-end. 
    
    Key insights:
        pattern match only as much as you need. The `(++)` definition only
        deconstructs the first list, making it lazy in the second:
-}

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

{-
    - Crucial point: It has produced the first element (`1`) without ever 
      looking at `[3, 4]` (the `ys`).

    Why is this cool? Because the second list (`ys`) can be infinite or 
    expensive to compute, and Haskell won't touch it until it finishes walking
    through the first list. 
-}

------ ------           ------ ------  ------   ------     ------ ------ ------

{-
    In Haskell, `Foldable` is a Typeclass. --> "Containers whose elements can be
    crunched down into a single value."

    You can't list every Foldable type (because you can write your own), but
    here are the standard:
    - List `[]`
    - `Maybe`: A container of 0 or 1 item. Folding it treats `Nothing` like an
      empty list `[]` and `Just x` like a single-item list `[x]`
    - `Either a`: Similar to `Maybe`; it folds over the `Right` value (the 
      "success" side) and ignores the `Left`.
    - `Tree` (from `Data.Tree` or custom definitions): You can fold a tree to
      visit every node (e.g. to find the max value in a binary search tree).
    - `Map` (from `Data.Map`): Folds over the values in the key-value pairs.

    Intuition: If you can write a `toList` function for a data type, it is 
    likely `Foldable`.

    ---

    The cons operator `(:)` adds to the front. This is always O(1) (instant), 
    because you don't have to walk anywhere; you just slap a new head on the
    list.

    *The Idiom:* If you need to build a list from scratch, build it backwards
        using `(:)` and then `reverse` it at the very end.

        - `reverse` is O(N) (once), which is much faster than the O(N^2) of 
          repeated appending.
                 

    - `:` (Cons) is O(1). Use this whenever possible.
    - `++` (Append) is O(length of left list)
      - Fine if the left list is short
      - Fine if you do it once at the end
      - Bad if you use it inside a loop to grow a list


-}


------ ------           ------ ------  ------   ------     ------ ------ ------

[n | n <- [0..10], even n]      -- filter
[even n | n <- digits]          -- map
[(x, y) | x <- xs, y <- ys]     -- cartesian product
        -- Logic: "For every `x` in the list `xs`, go through every `y` in the
        --        list `ys`, and combine them."
        -- Desugared: This turns into nested `concatMap` calls (or `listM2` in
        --        Monad terms.)

xs = [1, 2]
ys = [10, 20]
-- Result: [(1,10), (1,20), (2,10), (2,20)]

{-
    > List comprehensions are syntactic sugar for filtering/transforming.

    "Syntactic sugar" means nice syntax that compiles down to stanard functions
    like `map`, `filter`, and `concatMap`.
-}


------ ------           ------ ------  ------   ------     ------ ------ ------

{-
    1. `liftA2` vs `liftM2`

    Functionally, these two do almost the exact same thing, but they come from
    different eras of Haskell history.
    - `liftA2` (from `Control.Applicative`): Lifts a function with 2 arguments
      into an Applicative context.
    - `liftM2` (from `Control.Monad`): Lifts a function with 2 arguments into a
      Monad context.

    The Modern Rule: Since all Monads are now Applicatives (as of GHC 7.10),
    you should generally prefer `liftA2` because it works for more things. 
    `liftM2` is mostly kept for legacy code.

    
    2. What do they actually do?
-}

-- Imagine you have a normal function that adds two numbers:
add :: Int -> Int -> Int
add x y = x + y

{-
    Now imagine your numbers are trapped inside "boxes" (Contexts like `Maybe`,
    `List`, or `IO`). You can't just use `add`. You need to "lift" `add` so it 
    can reach inside the boxes.
-}

-- The signature
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c


-- Example 1: The `Maybe` Box
    -- You have `Just 5` and `Just 10`. You want to add them.
import Control.Applicative (liftA2)

val a = Just 5
val b = Just 10

result = liftA2 (+) val1 val2
-- Result: Just 15

    -- If either value was `Nothing`, the result would be `Nothing`.


{- 
Example 2: The List Box (Cartesian Product) This is where it gets powerful. It 
applies the function to every combination.
-}
list1 = [1, 10]
list2 = [2, 20]

result = liftA2 (+) list1 list2
-- Result: [11, 21, 12, 22]
-- (1+10, 1+20, 2+10, 2+20)


{-
Example 3: The IO Box
    Reading two lines from the user and combining them.    
-}
-- reads two lines and joins them
main = do
    combined <- liftA2 (++) getLine getLine
    putStrLn combined


{-
    3. Do `liftA3`, `liftM3`, etc. exist?

    Yes. ...
    - `liftA3` exists in `Control.Applicative`
    ...

    However, once you get past 2 or 3 arguments, people stop using `liftA...`
    functions because they become clumsy to write.

    Instead, Haskell programmers switch to "Applicative Style" using the 
    `<$>` (fmap) and <*> (apply) operators, which scales infinitely without 
    needing new function names.

    
    The Evolution:
    1. Standard Function: `f x y z`
    2. Using Lift (Arity 3): `liftA3 f mx my mz`
    3. Using Operators (The Idiomatic Way):
        - Idiomatic (English definition): using, containing, or denoting 
          expressions that are natural to a native speaker.
            * synonym: natural, native-speaker, grammatical
-}

f <$> mx <*> my <*> mz

{-
    The operator style means "Apply `f` to `mx`, then apply that result to `my`,
    then to `mz`." You can keep chaining `<*>` for as many arguments as you 
    need."

    --

    Summary
    1. `liftA2` applies a normal 2-argument function to two "wrapped" values.
    2. `liftM2` is the old Monad-specific version; prefer `liftA2` nowadays.
    3. Higher versions exist, but for 3+ arguments, it is cleaner to use the
      `<$>` and `<*>` operators.
-}


