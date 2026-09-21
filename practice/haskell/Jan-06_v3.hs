import Data.Maybe (catMaybes)

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)




transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

transpose' :: [[a]] -> [[a]]
transpose' = foldr (zipWith (:)) (repeat [])

        -- [[1, 2], [3, 4]]
        -- f 1 $ f 2 $ f z
        -- (zipWith (:)) [1, 2] $ (zipWith (:)) [3, 4] (repeat [])

{-
    This works by "injecting" each row into a set of infinite buckets.
-}




{-
                    ghci> zipWith (:) [[1, 3]] (repeat [])
                    [[[1,3]]]

                    ghci> zip [[1, 3]] (repeat [])
                    [([1,3],[])]

    Yes, they (`repeat :: a -> [a]`) effectively disappear in the case when 
    working with `zipWith` (and `zip`).


    THE GOLDEN RULE of ZIPPING

    `zipWith` stops as soon as the SHORTEST list runs out.


    ---

    WHY `repeat []` IS SAFE

    Haskell is LAZY. It doesn't generate the infinite list of empty lists in
    memory all at once. It only generates them "on demand."
    - When `zipWith` asks for the 1st item, `repeat` makes one `[]`.
    - When `zipWith` asks for the 2nd item, `repeat` makes another `[]`.
    - When `zipWith` stops asking, `repeat` stops working.
-}


-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.

-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.



{-
    1. `foldr` vs `foldl` (THE STRUCTURAL DIFFERENCE)

    The best way to learn these is not by their implementation, but by their
    EXPANSION.


    `foldr` (Fold Right)

    Think of `foldr` as CONSTRUCTOR REPLACEMENT. It takes a list like 
    `1 : 2 : 3 : 4 : []` and replaces the `:` with your function `f`, and
    the `[]` with your starting value `z`.

        - Definition: `foldr f z [1, 2, 3]`
        - Expansion: `f 1 `
-}

-- foldr: Right-assosciative
foldr' :: (a -> b -> b) -> b -> t a -> b
foldr' _ z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)
        -- f 1 $ f 2 $ f 3 acc


-- foldl: Left-assosciative
foldl' :: (b -> a -> b) -> b -> t a -> b
foldl' _ z []   = z
foldl' f z (x:xs) = foldl' f (f z x) xs
        -- f (f (f z 1) 2) 3





-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.

{-
    This is the DATA PROCESSING / ERROR HANDLING section of the exam. In the
    real world (and in Imperial exams), you often process a list of data where
    things might go wrong--parsing a CSV, validating user inputs, or looking
    up keys in a map.

    You end up with a list like `[Just "Data", Nothing, Just "More Data", Nothing]`

--------

    1. `catMaybes` (The Optimist)

        Philosophy: "Something is better than nothing. Give me what you have and
        ignore the errors."
            - Type: `catMaybes :: [Maybe a] -> [a]`
            - Action: Filters out `Nothing` and unwraps the `Just` values.
            - Result: A shorter list of pure values.

        Scenario: You are processing a list of User IDs. Some are valid, some
        are corrupted. You just want to email the valid ones.
-}

-- import Data.Maybe (catMaybes)

rawResults :: [Maybe Int]
rawResults = [Just 101, Just 100000, Nothing]

validIDs :: [Int]
validIDs = catMaybes rawResults


-------
-- Exam Implementation (Pattern Matching): If they ask you to write this from
-- scratch.

catMaybes :: [Maybe a] -> [a]
catMaybes []             = []
catMaybes (Just x : xs)  = x : catMaybes xs
catMaybes (Nothing : xs) = catMaybes xs




---
{-
    2. `mapMaybe` (The Efficient Pro)

    PROBABILITY: High (This is often a "Refactor this code" question).

    THE PATTERN: You often see code that maps a function and then calls 
    `catMaybes`. `catMaybes (map f xs)`

    This traverses the list TWICE. `mapMaybe` does it in ONE PASS.
        - Type: `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`

    EXAMPLE: "Parse a list of strings, keeping only the integers."
-}
-- import Data.Maybe (mapMaybe)
-- import Text.Read (readMaybe)

inputs = ["1", "2", "apple", "4", "banana"]

-- Amateur Way (Two Passes):
nums1 = catMaybes (map (readMaybe :: String -> Maybe Int) inputs)

-- Pro Way (One Pass):
nums2 = mapMaybe (readMaybe :: String -> Maybe Int) inputs

{-
                        ghci> mapMaybe (readMaybe :: String -> Maybe Int) ["1", "2", "apple", "4", "banana"]
                        [1,2,4]
                        ghci> map (readMaybe :: String -> Maybe Int) ["1", "2", "apple", "4", "banana"]
                        [Just 1,Just 2,Nothing,Just 4,Nothing]
-}



--------
{-
    3. `sequence` (The Perfectionist)

    PHILOSOPHY: "All or Nothing. If one part is broken, the entire dataset is
    invalid."

    - TYPE: `[Maybe a] -> Maybe [a]`        (Notice the flip!)
    - ACTION: It turns a "List of Maybes" into a "Maybe List"
    - BEHAVIOR:

            ...

    WHY IT WORKS (THE MONAD MAGIC): `sequence` isn't hardcoded for lists. It
    works because `Maybe` is a Monad where `>>=` stops on `Nothing`.

    EXAM IMPLEMENTATION (USING `foldr`): They love asking this because it tests
    if you understand Applicatives/Monads.


    ```Haskell
    sequence :: Monad m => [m a] -> m [a]
    sequence = foldr k (return [])
      where
        k m m' = do
            x <- m
            xs <- m'
            return (x:xs)
    ```


    TRACE: If `m` is `Nothing`, the `do` block stops immediately. `xs <- m'`
    never runs. The whole result becomes `Nothing`.



    ---

-}
-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.

{-
    These functions are all about FLIPPING STRUCTURES INSIDE OUT (turning a 
    "List or Maybes" into a "Maybe List").

    ---

    PART 1: THE TRAVERSABLE FOUR (The "Flip" Functions)


    These four functions do roughly the same thing. They differ only in HISTORY
    (Monad vs Applicative) and OPERATION (Do we map first?)

    THE CORE CONCEPT: Imagine you have a List of IO Actions (`[IO Int]`). You
    can't do math on that. You want an IO Action that returns a List 
    (`IO [Int]`). You need to pull the `IO` out to the front.


    1. `sequenceA` & `sequence` (The "Inside-Out" Flippers)
        - Input:  `t (f a)` (e.g., `[Maybe Int]`)
        - Output: `f (t a)` (e.g., `Maybe [Int]`)
        - Action: Runs the effects inside the container and collects the results.


    - `sequenceA`
        - Type Constraint: `Applicative`
        - Use When...  
            * You have a list of effects (`[Just 1, Just 2]`) and just want to
              flip it.
    - sequence`
        - Type Constraint: `Monad`
        - Use When...
            * You are working specifically with Monads (like IO).... but in modern haskell it's mostly the same as ``sequenceA`...


----------

    2. `traverse` & `mapM` (The "Map & Flip" Combo)

        - Input: A function `(a -> f b)` and a container `t a`.
        - Action: It maps the function over the container (creating `[f b]`),
          and then sequences it immediately.

         -- EQUIVALENCE sequence $ map f xs           ===     traverse f xs


        `traverse`
        - Type Constraint: `Applicative`
        - Use When...
            * You need to transform elements into effects and collect them.
        
        `mapM`
        - Type Constraint: `Monad`
        - Use When...
            * You are doing loops in `do` blocks (like IO).
-}





-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
{-
    PART 2: THE SCAN FUNCTIONS (THE "RUNNING TOTAL")

    `fold` reduces a list to one value. `scan` does the exact same thing, but
    KEEPS THE HISTORY of every intermediate step.


    1. `scanl` (Scan Left - The "Cumulative Sum")

            1. scanl (Scan Left - The "Cumulative Sum")
                Analogy: Walking down a list and writing down the running total.

                Direction: Left to Right.

                Length: Input + 1 (It always includes the starting accumulator).

                Haskell

                -- scanl function start_acc list
                scanl (+) 0 [1, 2, 3]

                -- Trace:
                -- 1. Start: [0]
                -- 2. 0 + 1 = 1 -> [0, 1]
                -- 3. 1 + 2 = 3 -> [0, 1, 3]
                -- 4. 3 + 3 = 6 -> [0, 1, 3, 6]

                -- Result: [0, 1, 3, 6]


    
    2. `scanr` (Scan Right -- The "Suffix Sum")
        - ANALOGY: This is the brain-twister. It processes from the RIGHT, but
          the result is still a list ordered from left to right. It represents
          the "reduction of the suffix starting at this point".




-}













{-
    2. THE MAIN EVENT: `freeVars`

    EXPLANATION: We need to traverse the AST and apply the rules of Scope.
    
    1. CONST: Numbers have no variables.
    2. VAR: A variable is free unless it is a PRIMITIVE (like `+` or `ite`). We
       must check the `prims` list.
    3. FUN: The body might use global variables. However, the ARGUMENTS of the
       function are bound (local), so we must SUBTRACT (`(\\)`) them from the           -- import Data.List (\\)
       result.
    4. APP: A function call `f a b` uses variables from the function `f` AND all
       arguments `a, b`. We just union them all.
    5. Let (The Tricky One):
        - `Let [("x", e1)]`

-}

            -- always ask yourself how can you use functions from existing previously defined ones...

-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.


{-
    FILL THIS OUT BEFORE EXAM


                SUMMARY CHEAT SHEET

                        Operation               List (`Data.List`)              Set (`Data.Set`)                Map (`Data.Maps`)
                        Convert from List       `id`                    
                        Check exists
                        Access Item
                        Insert
                        Delete
                        Filter
                        Map

                        CCA IDF M <-- convert from list // check exists // access item // insert // delete // filter // map



    ALSO FOCUS ON DOING QUIZZES PROVIDED BY GEMINI... THEY ARE ACTUALLY VERY USEFUL!
-}

{-
    0.
        PPT 3, 4, 5 LEFT
        - graph algorithm practice

    0.2. 
        FINISH MONDAY + TEUSDAY REVISION lecture sessions ... AND ASK GEMINI TO CREATE NOTES FROM IT


    1.

       -- target 2021... need to use `zip` and `zipWith`
                -- need to also watch monday 4 hr lectures... likely will just pass it into AI... need to also practice with foldl and foldr

       -- 2022 is hard
       -- 2025 seem to be about parsers... or state monads

                `nub`
                \case
                [ParseTree]
                split2 and split3.          <-- custom PPQ function... ig we will learn this more in ppq... but pay attention to function re-use...

        == UNASSESSED EXERCISES NEXT
        

    2. still don't understand the `commands` and `commands'` functions from bottom of ApplicativeParsing.lhs
            -- A potential concern rn tbf... is that we are very good at implementation of interfaces rn... but not much good at
               actually using these functions and HOFs... but this should be fixable in the next 3 days

    3. need to practice with `do` notation for for-loops and LCs and in general...
            as in... also using the feature where we directly also engage with >> and `putStrLn` for instant debug!
                        have to get used to writing these monads myself in practice too...
                                        In Haskell, all of these are just Monads:
                                                        Looping? List Monad.
                                                        Null checks? Maybe Monad.
                                                        Error handling? Either Monad.
                                                        Async/Side-effects? IO Monad.
                                        https://gemini.google.com/app/fc93170cd93f6546
                                        https://gemini.google.com/app/56a9

    4. check again the command to disable overwrite mode... and to trigger the 80-columnth guideline...


    ADDITIONAL
        - check what was the interrim incredible monad technique useds




    THINGS HINTED TO BE NEEDED IN THE EXAM
    - `nub :: Eq a => [a] -> [a]`                       <-- `import Data.List` required
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id


-- Definitely don't forget to use :doc
        -- :doc, :info, :kind, :type, :browse



        -- for list...
            -- `delete`, `(\\)`, `filter (/= x) xs`
            -- unzip, zip, zipWith
            -- wow you can also do predicates too inside list comprehension... completely forgot about this...

        -- look out for the With functions...




{-
    Data.List
    Data.Maybe
    Data.Map
    Data.Set

    Data.Tuple
    Data.Char
    Data.Ord

    Data.Foldable
    Data.Traversable

    Control.Applicative
    Control.Monad

    Text.Read

-}