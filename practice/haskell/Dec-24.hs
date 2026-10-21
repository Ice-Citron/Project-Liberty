-- 2. Custom datatypes

data MyBool = MyFalse | MyTrue

data Date where
    Date :: Day -> Month -> Year -> Date

-- Constructors can take arguments. Pattern match to deconstruct:

greeting (Date 25 Dec _) = "Happy Christmas!"

------ ------           ------ ------  ------   ------     ------ ------ ------


-- 3. Typeclasses -- the truth about numbers
{-
    `7` doesn't have type `Int`. It has type `Num a => a`. The constraint 
    `Num a =>` means "for any type `a` that's a number."

    Key typeclasses to know:
    - `Num` - `(+)`, `(-)`, `(*)`, `abs`, `negate`
    - `Integral` - `div`, `mod` (integer division)
    - `Fractional` - `(/)` (exact division)
    - `Eq` - `(==)`, `(/=)`
    - `Ord` - `(<)`, `(>)`, `(<=)`, `(>=)`
    - `Show` - `show` (convert to String) 
-}

sqaure :: Num a => a -> a
sqaure x = x * x

------ ------           ------ ------  ------   ------     ------ ------ ------


-- 4. `let` vs `where`
    -- Both bind variables, different scopes:

-- where: belongs to whole definition
power n x
  | even n    = x' * x'
  | otherwise = x * x' * x'
  where m = n `div` 2
        x' = power m x

-- let: expression-level, more precise scope
let x' = power m x in x' * x'           

------ ------           ------ ------  ------   ------     ------ ------ ------


{-
    1. `liftA2` vs `zipWith`

    They both combine two lists, but they use different logic:

    - `zipWith`: Pairs items index-by-index. (1st with 1st, 2nd with 2nd). Stops
      at shorter list.
    - `liftA2`: Creates the Cartesian Product. (Every item in list A mixed with
      every item in list B).
-}

xs = [1, 2]
ys = [10, 20]

-- zipWith: "Zipper" logic
zipWith (+) xs ys
-- Result: [11, 22]      (1+10, 2+20)

-- liftA2: "All Combinations" logic
liftA2 (+) xs ys
-- Result: [11, 21, 12, 22]     (1+10, 1+20, 2+10, 2+20)


{-
    2. Clearing up your confusion: `<$>` and `<*>`
    
    > "is it that f for <$> only takes in one value... or 3 values?"

    The Answer: `f` takes 3 values total.

    The operators `<$>` and `<*>` feed the arguments to `f` one by one. This
    relies on `Currying` (the fact that Haskell functions take one argument at
    a time and return a new function awaiting the rest).


    The Step-by-Step Visualisation

    Imagine if `f` is `\x y z -> x + y + z` (it needs 3 numbers).

    1. `f <$> Just 1`

        - `Maybe (Int -> Int -> Int)`

    2. `... <*> Just 2`    

        - `Maybe (Int -> Int)`

    3. `... <*> Just 3`

        - `Just 6`
-}


------ ------           ------ ------  ------   ------     ------ ------ ------

-- 3. Real-World Use Cases

{-
    ... for Parsing and Data Construction

    Use Case A: Constructing Data Types (The most common!)

    You have a data type `User` that needs a Name, an Age, and an Email. You
    have 3 separate "parser" or "validators" that fiond these values.
-}

data User = User String Int String deriving Show

-- Assuming these returns 'Maybe String' or `Maybe Int`
val name = Just "Alice"
val age = Just 20
val email = Just "alice@c.com"

-- Build the User!
-- "User" is the function "f". It takes 3 arguments.
myUser = User <$> name <*> age <*> email
-- Result: Just (User "Alice" 20 "alice@c.com")

-- If any of those were `Nothing`, the whole result becomes `Nothing` 
-- automatically.


{-
    Use Case B: Doing Math with Optionals

    You want to add 3 numbers, but any of them might be missing (null/Nothing).
-}
val x = Just 10
val y = Just 20
val z = Nothing     -- Oops, missing data

total = (\ a b c -> a + b + c) <$> x <*> y <*> z
-- Result: Nothing

-- This saves you from writing nested `case` or `if-then-else` statements
-- checking for Nulls.


{-
    Use Case C: Input/Output (IO)

    Reading 3 lines from the user and combining them into one string
-}
main = do
    -- Run 3 getLines, feed results one-by-one
    result <- (\x y z = x ++ y ++ z) <$> getLine <*> getLine <*> getLine
    putStrLn result





------ ------           ------ ------  ------   ------     ------ ------ ------
-- 1. The Great Lie: Currying

{-
    Functions don't have multiple arguments. `add :: Int -> Int -> Int` is 
    actually `add :: Int -> (Int -> Int)`. A function that takes one Int and 
    returns another function.
-}
add x y = x + y
add x = \y -> x + y     -- equivalent

-- This enables partial application:
addFive = add 5



-- 2. Function Composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

-- Pipeline: `chr . (+ n) . ord` means "convert to Int, add n, convert back to 
-- Char"



-- 3. The Big Three: map, filter, concatMap
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f [] = []
concatMap f (x:xs) = f x ++ concatMap f xs

-- List comprehensions to desugar these:
{-
    - [f x | x <- xs] = map f xs
    - [x | x <- xs, p x] filter p xs
    - [(x, y) | x <- xs, y <- ys] = concatMap (\x -> map (\y -> (x, y)) ys) xs
-}



-- 4. Folds -- the ultimate generalization

-- `foldr` replaces `(:)` with `f` and `[]` with `k`:
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f k [] = k
foldr f k (x:xs) = f x (foldr f k xs)

-- Visually:
foldr f k [1, 2, 3] = 1 `f` (2 `f` (3 `f` k))


-- Everything is a fold:
sum = foldr (+) 0
product = foldr (*) 1
and = foldr (&&) True
concat = foldr (++) []
map f = foldr (\x ys -> f x : ys) []
length = foldr (const (+1)) 0

-- Fold-map fusion: `foldr f k . map g = foldr (f . g) k`


-- 5. Tail Recursion & foldl
    -- `foldr` builds up thunks. `foldl` uses an accumulator:
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = k
foldl f acc (x:xs) = foldl f (f acc x) xs

-- Visually:
foldl f k [1, 2, 3] = ((k `f` 1) `f` 2) `f` 3

reverse = foldl (flip (:)) []



------ ------           ------ ------  ------   ------     ------ ------ ------
-- 6 . The Strictness Trap

-- The critical issue: `foldl` still builds thunks because Haskell is lazy!
foldl (+) 0 [1,2,3]
= go (0+1) [2,3]
= go ((0+1)+2) [3]
= go (((0+1)+2)+3) []  -- thunk explosion!

-- Solution: bang patterns force evaluation:
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !acc [] = acc
foldl' f !acc (x:xs) = foldl' f (f acc x) xs

------ ------           ------ ------  ------   ------     ------ ------ ------

{-
    Step 4: `foldl' (+) (((0+1)+2)+3) []`
    - Matches Base Case: `foldl' f !acc [] = acc`
    - Bang triggers: NOW the compiler forces the accumulator.
    - It has to evaluate the entire giant expression `(((0 + 1) + 2) + 3)` all
      at once.

    The Result: You still used O(N) memory to store the thunks. If the list was
    1 million items long, you would crash with a Stack Overflow before you hit
    the base case (or right as you hit it).  
    


    2. The Fix: Bang the Recursive Step

    To actuallyu get the benefits of `foldl'`, you must force the accumulator
    before you enter the next recursive call.

    
    Option A: Bang the pattern in the recursive case
-}
-- This ensures `acc` is evaluated BEFORE the function body runs
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !acc [] = acc
foldl' f !acc (x:xs) = foldl' f (f x acc) xs

{-
    Note: In this specific style, `!acc` forces the incoming `acc`. But strictly
    speaking, we also need to ensure the new accumulator `(f acc x)` is forced
    before the next recursion.

    Actually, the most robust way to write `foldl'` (and how GHC implements it)
    uses `seq` or a specific bang placement to force the NEXT state:
-}

-- Option B: The Standard Implementation
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !acc [] = acc
foldl' f !acc (x:xs) = 
    let nextAcc = f acc x
    in nextAcc `seq` foldl' f nextAcc xs

-- Or with Bangpatterns on the `let` (if enabled):
foldl' f !acc (x:xs) =
    let !nextAcc = f acc x
    in foldl' f nextAcc xs
------ ------           ------ ------  ------   ------     ------ ------ ------


-- Week 5

-- 1. Streams (Infinite Lists)
    -- Notes show some elegant definitions using laziness:

repeat :: Int -> [Int]
repeat x = x : repeat x

-- more efficient version using sharing
repeat' :: Int -> [Int]
repeat' x = xs
  where xs = x : xs

{-
    The second version is more efficient because `xs` refers to itself--there's
    only one cons cell in memory, pointing back to itself.
-}

{-
    Your lecturer is trying to teach you about Knot-Tying (Cyclic Data 
    Structures) and Data Flow.

    ---

    1. Why "Streams" (Infinite List)?

    The Core Concept: Separation of Concerns.

    In other languages (like C or Java), if you want the first 5 primes, you
    write a loop that runes 5 times. If you want the first 100, you change the
    loop to 100. The logic for "generating numbers" and "deciding when to stop"
    is mixed together.

    In Haskell, Streams let you separate them:

    1. The Producer: "Here is an infinite list of all primes." (It doesn't care
      if you need 5 or 5 million).
    2. The Consumer: "I'll take the first 10 from that infinite pile."

    Why is this good? It makes your code modular. You can reuse the "infinite
    primes" definition everywhere, regardless of how many you actually need in
    that specific moment.

    ---

    2. The "Weird" Effuciency Trick

    Version 1: The "Naive" Way

    ```Haskell
    repeat x = x : repeat x
    ```

    Everytime you ask for the next item, Haskell runs the function `repeat` 
    again. It allocates a new memory cell (a new box) for that list node.
    - Memory: `[x] -> [x] -> [x] -> [x] ...` (Infinite new boxes being created 
      as you walk down the list).


    Version 2: The "Shared" Way
    
    ```Haskell
    repeat x = xs where xs = x : xs
    ```

    Here, `xs` is a named value (a pointer). Haskell defines `xs` as "a cell
    containing `x`, whose tail points... back to `xs`."
    - Memory: [x] (with an arrow curving back to itself).
    - Result: It takes up O(1) constant memory, no matter how much of the list
      you look at. It's literally a circle in RAM.
-}


-- Streams as Num instances (this is clever):
nats :: [Integer]
nats = [0..]

instance Num [Integer] where
    fromInteger x = repeat x        -- literal becomes infinite stream
    xs + ys = zipWith (+) xs ys     -- pointwise addition

-- The famous one-liner Fibonacci:
fibs :: [Integer]
fibs = 0 : 1 : (fibs + tail fibs)
    -- This works because of laziness--we only compute as much as we need.

{-
    3. Lists as Numbers (The `Num` Instance)

    The code block `instance Num [Integer]` is redefining what `+` means for 
    lists.

    Normally, `+` doesn't work on lists. Your lecturer decided: "When I add two
    lists, I want to add them item-by-item."

    Visualising the Code: `xs + ys = zipWith (+) xs ys`

    Imagine two conveyor belts moving in parallel:
    - Stream A: [1, 1, 1, ...]
    - Stream B: [2, 3, 4, ...]
    - A + B: [3, 4, 5, ...]

    This is required for the Fibonacci magic...


    What is happening under the hood:
    1. Haskell knows `fibs` starts with `[0, 1]`.
    2. It needs the 3rd element. The formula says: `head fibs + head (tail 
        fibs)`
    3. That is `0 + 1 = 1`. So `fibs` is now `[0, 1, 1]`.
    4. It needs the 4th element. The formula slides down one slot.
    5. It takes the 2nd element of `fibs` (`1`) and the 2nd element of 
        `tail fibs` (`1`).
    6. `1 + 1 = 2`. So `fibs` is now `[0, 1, 1, 2]`.

    Summary: The stream is feeding its own output back into its input to
    calculate the future. It is a feedback loop.
-}

------ ------           ------ ------  ------   ------     ------ ------ ------

-- 2. Maybe -- Safe Partial Functions

{-
    `head :: [a] -> a` crashes on empty lists. We fix this with `Maybe`:

    ```Haskell
    data Maybe a = Nothing | Just a

    safeHead :: [a] -> Maybe a
    safeHead []    = Nothing
    safeHead (x:_) = Just x

    toList :: Maybe a -> [a]
    toList Nothing  = []
    toList (Just x) = [x]
    ```

    Example values:
    ```Haskell
    Nothing :: Maybe a      -- polymorphic "absence"
    Just True :: Maybe Bool
    Just 3 :: Maybe Int
    Just Nothing :: Maybe (Maybe a)         -- Nested!
    ```
-}


-- 3. Either -- Errors with Information
    -- `Maybe` tells you if something failed. `Either` tells you why:
data Either a b = Left a | Right b

safeHead' :: [a] -> Either String a
safeHead' []     = Left "Empty String"
safeHead' (x: _) = Right x

-- Convention: `Left` = error/failure, `Right` = success 
-- (mnemonic: "right" = correct)

Right 12 :: Either String Int       -- success with Int
Left "Hello" :: Either String Int   -- failure with mesage
Right 12 :: Either a Int            -- `a` can be anything (not used)

-- Note: `Right 12 :: Either a b` doesn't work--`b` must be `Int`, but `a` is
-- unconstrained only when `Left` isn't used.



------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------

-- 4. Expression Trees with Environments

data Expr = Val Int | Add Expr Expr | Var String

eval :: (String -> Int) -> Expr -> Int

{-
    The first argument is an environment--a function mapping variable name to
    values. In practice, often represented as `[(String, Int)]` with `lookup`.
-}


{-
    This code is trying to teach you how to build a programming language (or at
    least a simple calculator) from scratch.

    It introduces two core concepts of compiler design:

    1. Syntax (The Shape): How we represent code as data (Expr).
    2. Semantics (The Meaning): How we actually run that code (`eval`).

    ---

    1. What is `data Expr`?

    This is an Abstract Syntax Tree (AST). it is a recursive data structure that
    describes the shape of a mathematical expression, but it doesn't calculate
    anything yet.

    ```Haskell
    data Expr = Val Int | Add Expr Expr | Var String
    ```

    - `Val Int`: Represents a raw number (e.g. `5`).
    - `Var String`: Represents a variable name (e.g. `"x"`).
    - `Add Expr Expr`: Represents the concept of adding two things together.


    2. What is the "Data Type" of `Add`?

    This is a common point of confusion. `Add` is not a Type; it is a 
    Constructor.

    - The Type is `Expr`
    - `Add` is a function that creates an `Expr`.

    If you checked the type of `Add` in GHCi...

    ```Haskell
    Add :: Expr -> Expr -> Expr
    ```

    Translation: "`Add` is a function that takes two `Expr`s (a left side and a
    right side) and wraps them up into a new, bigger `Expr`."


    3. What is the purpose of `Add Expr Expr`?

    Its purpose is to store the structure of an addition operation so you can
    process it later. It captures the nesting of math.

    ...

    This tree structure allows your program to "look" at the math equation as
    data.
-}

--  4. What is `eval` trying to teach me?
    -- The `eval` function teaches you how to interpret (run) that tree.

eval :: (String -> Int) -> Expr -> Int

{-
    The confusing part is likely the first argument: `(String -> Int)`. This is
    the Environment (or "Lookup Table").

    Why do we need it? If i give you the expression `x + 5` (which is 
    `Add (Var "x") (Val 5)`), and I tell you "Calculate this", you would scream,
    "What is x?!"

    The Environment answers that question. It is a function that takes a 
    variable name (`String`) and tells you its value (`Int`).
-}

-- How it works in practice:

-- 1. Define an environment (a lookup function)
myEnv :: String -> Int
myEnv "x" = 10
myEnv "y" = 20
myEnv _   = 0       -- default for others

-- 2. Define an expression: "x + 5"
myCode :: Expr
myCode = Add (Var "x") (Val 5)

-- 3. Run it!
result = eval myEnv myCode
-- Result: 15

{-
    What happens inside `eval`:
    1. `eval` sees `Add`. It splits right there.
    2. It evaluates the left side: `Var "x"`. It uses `myEnv "x"` to get 10.
    3. It evaluates the right side: `Val 5`. It just extracts `5`.
    4. It adds them: `10 + 5 = 15`

    Summary:
    - `Expr` is the Blueprint of the math.
    - `Add` is the Glue holding pieces of the blueprint together.
    - `eval` is the Builder that reads the blueprint and calculates the result.
    - The Environment is the Reference Manual that tells the builder what 
      variables like "x" actually equal.
-}


------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------
------ ------           ------ ------  ------   ------     ------ ------ ------
