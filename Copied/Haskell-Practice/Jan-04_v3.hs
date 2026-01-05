import Data.Foldable (forM_)       -- Works for Lists, Sets, Maps, etc.
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
    If STATE is out, but you want to understand `do` notation "imperatively"
    with data structures, the TREE MONAD is actually a fantastic example.

    It shjares the exact same "magic" as the LIST monad (which your classmates
    mentioned) but for a branching structure/



    THE CONCEPT: "LEAF SUBSTITUTION"

    When we write imperative-style code with a Tree Monad:#
        1. `x <- tree` means: "Focus on EVERY LEAF VALUE, one by one.s"
        2. The lines after that define what to replace that Leaf with.
        3. `return y` means: "Create a new Leaf containing `y`."

    It is basically a FIND-AND-REPLACE loop for the entire tree strucure.


    ---


    1. THE SETUP (THE TREE)

    Let's define a simple binary tree that has values only at the leaves (this 
    is the standard "Monad" tree).
-}

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node lt rt) = Node (fmap f lt) (fmap f rt)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf                           -- eta-reduced form!

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f)     <*> t = fmap f t                     -- f <$> t
  (Node lf rf) <*> t = Node (lf <*> t) (rf <*> t)
 
{-                                                                  
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f)     <*> (Leaf x)     = Leaf (f x)                        --
  (Leaf f)     <*> (Node lt rt) = Node (f <$> lt) (f <$> rt)        -- PM 1 and PM2 can be significantly simplified instead as seen below
  (Node lf rf) <*> (Leaf x)     = Leaf (lf x)                               -- this verion here is "zip with two trees"... but isn't ideal here and we need to instead use the version below... because it doesn't allign with our implementation of `interface Monad`
  (Node lf rf) <*> (Node lt rt) = Node (lf <*> lt) (rf <*> rt)
-}

instance Monad Tree where                                                       -- this instance allows us to use the `do` notation
    return :: a -> Tree a
    return = Leaf                                                           -- return: Wraps a value into the simplest Tree (a Leaf)   // eta-reduced! 

    (>>=) :: Tree a -> (a -> Tree b) -> Tree b                                  -- (>>=): "For each leaf, run the function k"
    (Leaf x)     >>= f = f x
    (Node lt rt) >>= f = Node (lt >>= f) (rt >>= f)



{-
    2. THE "IMPERATIVE" LOGIC

    Imagine you have a small tree of inputs: `(1) -- (2)`. You want to write a 
    script that says: "For every value, if it's even. turn it into a tiny
    sub-tree of its neighbors. If it's odd, leave it alone."
-}

expandEvenNumbers :: Tree Int -> Tree Int
expandEvenNumbers tree = do
    x <- tree                   -- 1. Extract the value from the current leaf           // this actually works because >>= functions as fmap as it iterates across all values inside `tree`!!! ALWAYS REMEMBER THAT >>= === concatMap for lists === join fmap
    case x of                   -- 2. Imperative-style check
      _ | even x    -> Node (Leaf (x-1)) (Leaf (x+1))           -- 3. "Grow" a new branch here
        | otherwise -> return x                                 -- 4. Just return the value as is (keep the leaf)



{-
    4. WHY IS THIS "IMPERATIVE"?

    You didn't have to manually traverse the tree. You didn't have to write
    recursion (`Node (expand 1) (expand r)`).

    You just wrote a script from the perspective of "What do I do with a single
    value `x`?"... and the Monad handled the recursion and reconstruction of the 
    tree for you.




    SUMMARY FOR EXAMS

    If you see `do` notation with a data structure (List, Tree, Maybe):
    1. `x <- structure`: Reads as "For every item `x` inside this structure..."
    2. `func x`: "Transform `x` into a new structure..."
    3. STRUCTURE: The Monad handles the glue (looping, recursion, stitching)
       automatically.


-}




{-
    2. THE "WHILE LOOP" (RECURSION)

    A `while` loop in a Tree usually means "Keep digging down until you hit a
    condition."

    Since Trees are recursive data structures, "while loops" are just recursive
    functions. You don't rely on `do` notation's implicit looping here; you 
    often write the recursion explicitly to control which branch you follow.

    SCENARIO: We want to simulate a "Binary Search Tree" lookup imperative-style
    . "While the current node is not the target, go left or right."
-}

-- A different kind of tree (Values at branches, not just leaves)
data BST a = Empty | NodeBSTs a (BST a) (BST a)

-- "While Loop" to find a value
search :: (Ord a) => a -> BST a -> Bool
search target currentTree = 
    case currentTree of
        Empty                  -> False
        NodeBST val left right ->
            if val == target
                then True
                else if target < val
                    then search target left     -- "Loop" left
                    else search target right    -- "Loop" right

                                    --  Note: This standard recursion is cleaner than do notation for this specific "drill-down" logic.


                





------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
{-
   _ _  _ _    _ _         Y                        _ _
  ( ) )( ( )  ( ) )       ((                       ( ( )
 (  o  ) o  )(  o  )       ))        .---------.  (  o  )
  (_(_)(_)_)  (_(_)     ,-'''-,      | Here Be |   (_)_)
 |\ | /| | /||\ | /|   ( |\_/| )     | Dragons |  |\ | /|
  \\|//\\|//  \\|//    \\(q p)//     '----,----'   \\|//
___\|/__\|/____\|/_______M<">M____________|_________\|/_____
-}
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---

{-
    The reason this feels weird is that you are used to `do` notation meaning
    "Perform Action A, then Action B" (sequential).

    But when you use `do` with LISTS. the meaning changes. It effectively means:
    "Perform ths for EVERY item."

    ... breakdown below of how to translate your mental "For Loops" and "List
    Comprehensions" into Monads.



    ---

    1. The "List Comprehension" to "Do" Translation

    List comprehensions are actually just syntactic sugar for the List Monad.

    THE LOGIC: In the List Monad, `x <- list` means "Let `x` be EACH ELEMENT of
    the list, one by one."



                Scenario: Generate Cartesian Product
                        We want to combine [1, 2] and ['a', 'b'] to get [(1,'a'), (1,'b'), (2,'a'), (2,'b')].

                        A. Python (Imperative Loop)

                        Python

                        results = []
                        for number in [1, 2]:
                            for letter in ['a', 'b']:
                                results.append((number, letter))
-}

-- Haskell (List Comprehension)
results = [ (n, l) | n <- [1, 2], l <- ['a', 'b'] ]

-- Haskell (Do Notation)      // This runs the exact same logic as the Python Loop
results' :: [(Int, Char)]
results' = do
    n <- [1, 2]
    l <- ['a', 'b']
    return (n, l)                   -- <-- don't forget! because of the fact that (>>=) is also concatMap... or join (fmap f mx)... this actually mean all the individual singleton [(Int, Char)] actually ends up being concatenated together!!!
                                                -- You make many tiny buckets (singleton lists). The Monad takes all your tiny buckets
                                                -- and pours them into one big bucket at the end.
                                                                -- return x = [x] for lists... non eta-reduced...

{-
    HOW IT WORKS (The `>>=` operator)

    Under the hood, the `do` notation rewrites your code into `>>=`(bind). For
    lists, `>>=` is defined effectively as `concatMap`.

            -- The 'do' block above becomes:
            [1, 2] >>= (\n -> 
                ['a', 'b'] >>= (\l -> 
                    return (n, l)
                )
            )
            It literally maps your function over the list and flattens the result. That is a loop.


    
    ----


    2. The "Side Effect" Loop (forM_)
        The previous example was for generating data (pure). If you want to do things (like print to console, or update State) using a list/map/set as the source, you use forM_.
        forM_ is the closest thing Haskell has to a traditional for keyword.
        Scenario: We have a list of User IDs. We want to print them.
-}





------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---


------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---


{-


You hit the nail on the head again. The "stitching" is exactly what is happening, but there is one tiny specific detail you need to see to make it click.

You are confused because you are thinking `return (n, l)` produces a raw tuple `(n, l)`.
If that were true, you'd be right—how do you join a bunch of raw tuples into a list?

The secret is in the definition of `return` for Lists.

### The Missing Piece: `return` creates a List

In the List Monad, `return` is defined as:

```haskell
return x = [x]  -- It wraps the value in a singleton list!

```

It **does not** simply return the tuple. It returns a **one-item list containing the tuple**.

### The "Stitching" Visualized

So, when your code runs `return (n, l)`, it is actually generating small, disconnected lists like `[(1, 'a')]`.

The `do` block (and the underlying `>>=` operator) is effectively saying:
**"Run this code for every item, and then `++` (concatenate) all the resulting little lists together."**

Here is the slow-motion replay of your code:

**1. The Inner Loop runs for `n=1`:**

* `l='a'`  `return (1, 'a')`  **Result:** `[(1, 'a')]`
* `l='b'`  `return (1, 'b')`  **Result:** `[(1, 'b')]`

**2. The Inner Stitching (The Bind):**
The inner bind (`>>=`) takes those small results and stitches them:
`[(1, 'a')] ++ [(1, 'b')]`
**Result:** `[(1, 'a'), (1, 'b')]`

**3. The Outer Loop runs for `n=2`:**

* Same process happens...
* **Result:** `[(2, 'a'), (2, 'b')]`

**4. The Outer Stitching:**
The outer bind takes the result from step 2 and step 3 and stitches them:
`[(1, 'a'), (1, 'b')] ++ [(2, 'a'), (2, 'b')]`

**Final Result:** `[(1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')]`

---

### Comparison to Python

* **Python (Mutation):** You have *one* big bucket (the list). You pick up a tuple and throw it into the bucket.
* **Haskell (Monoid/Concat):** You make many tiny buckets (singleton lists). The Monad takes all your tiny buckets and pours them into one big bucket at the end.

So, `return` creates the "tiny bucket," and `<-` (bind) pours them together.
-}

-- import Data.Foldable (forM_)

printUsers :: IO ()
printUsers = do
    let userIDs = [101, 102, 103]
    forM_ userIDs $ \uid -> do                            -- \uid represents \x in the case of >>=... but it's forM_ instead
        putStrLn $ "Processing User ID: " ++ show uid
        -- you can do more lines here




{-
    3. LOOPING OVER MAPS AND SETS

    Tricky... because Maps and Sets are not Monads in the standard Haskell way
    (because of type constraints). You cannot write `x <- mySet`.

    However, they are FOLDABLE. This means you can loop over them using `forM_`                        -- import Data.Foldable (forM_)
    just like a lsit!
-}

-- A. Sets (Iterating over unique items)
-- import qualified Data.Set as Set
-- import Data.Foldable (forM_)

processSet :: IO ()
processSet = do
    let uniqueNums = Set.fromList[1, 2, 3, 2, 1]        -- becomes {1, 2, 3}
    putStrLn "Startin Set Loop:"
    forM_ uniqueNums $ \num -> do
        putStrLn $ "Number: " ++ show num



{-
    B. Maps (Iterating over Values vs Keys)
        When you use forM_ on a Map, it iterates over the values only (ignoring keys).

        If you want the standard for key, value in map loop (like Python), you must convert the Map to a list of pairs first using Map.toList.
-}
-- import qualified Data.Map as Map
-- import Data.Foldable (forM_)

processMap :: IO()
processMap = do
    let scores = Map.fromList [("Alice", 10), ("Bob", 8)]
    
    -- LOOP 1: Values only (Implicit)
    putStrLn "-- Values Only --"
    forM_ scores $ \score -> do
        print (score * 2) 
        
    -- LOOP 2: Keys and Values (Python style)
    putStrLn "-- Key and Value --"
    -- We convert to list: [("Alice", 10), ("Bob", 8)]
    forM_ (Map.toList scores) $ \(name, score) -> do
        putStrLn $ name ++ " scored " ++ show score


------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---


{-
    HASKELL TIC-TAC-TOW
        -- looks like we just need to learn these wide range of custom variables from these Data.* libraries
                    -- uses `replicate` to create n^2 Empty cells.     `[100 | _ <- [1..10]]` === `do _ <- [1..10]; return 100`
                    -- `nub` in Data.List
                            -- very smart... if `nub` returns a singleton `[Taken 0]` or `[Taken X]` (which means that there's
                                no other pieces present... because nub only returns special pieces)... this means the whole line 
                                is that players' markers.
                    -- `elem :: (Foldable t, Eq a) => a -> t a -> Bool`
                    -- `any`, `all`
                    
                    ```Haskell
                    parsePosition :: String -> Maybe Position
                    parsePosition s = case words s of
                        [r, c] -> (,) <$> readMaybe r <*> readMaybe c              -- <-- (,) * <$> * <*> * ... is how one turns 2 elements into tuples!
                        _      -> Nothing                                          -- combines two `Maybe Int` into `Maybe (Int, Int)`... tbf probably could've used liftA2 instead
                    ```

                    -- row-major indexing: ...      `where idx = r * n + c`
                    -- used helper function `showcell`


                    ```Haskell
                    doParseAction :: String -> (String -> Maybe a) -> IO a
                    doParseAction errMsg parser = do
                        input <- getLine
                        case parser input of
                          Just x  -> return x
                          Nothing -> putStr errMsg >> doParseAction errMsg parser       <-- incredible... `>>` and recursion of `doParseAction` used...
                    ```
                    -- Key Technoque: Recursive retry pattern for IO with Maybe Validation
                        -- `getLine :: IO String`
                    
                    ```Haskell
                    takeTurn b p = do
                      putStr $ "Player " ++ show p ++ ", make your move (row col): "
                      doParseAction "Invalid move, try again: " tryParseAndMove
                      where
                        tryParseAndMove s = parsePosition s >>= \pos -> tryMove p pos b
                    ```
                    -- Key Technique: Chains `parsePosition` and `tryMove` with `>>=` (both return Maybe).


                    ```Haskell
                    playGame b p = do
                      prettyPrint b
                      newBoard <- takeTurn b p
                      if gameOver newBoard
                        then prettyPrint newBoard >> putStrLn ("Player " ++ show p ++ " has won!")
                        else playGame newBoard (swap p)
                    ```




                --------
                    DON'T understand
                    - prettyPrint
                        - explain to me what does mapM_ do... show example... and why `(.)` with intersperse?
                        - what does `intersperse` do?

                    - how to convert various types among each other
                        - forgot what does `fromIntegeral`
                        - i remember `show`...
                                    





-}



{-
    0.
        PPT 3, 4, 5 LEFT


    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

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
        - Claude chat - Four Approaches to LSystem Parsing
                            https://claude.ai/chat/9189b553-f13c-4cd2-90e2-cd9243d88d30
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id