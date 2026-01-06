import Data.Foldable
import Data.Traversable         -- importing forM_ and mapM_

prettyPrint :: Board -> IO ()
prettyPrint b = do
  let myRows = rows b               -- 1. Grab the rows (The data source)
  forM_ myRows $ \i -> do           -- 2. Start the "Loop"
    let cellStrings   = map showCell row                -- A. Convert the cells in this specific row to Strings
    let formattedLine = intersperse " " cellStrings     -- B. Insert the spaces (The formatting logic)
    putStrln formattedLine                              -- C. The actual IO action (printing)



ghci> import qualified Data.Map as Map
ghci> let ms = Map.fromList [("a", 33), ("b", 27)]
ghci> :type ms
ms :: Num a => Map.Map String a
ghci> let ms2 = Map.fromList [("a", 33), ("b", 27)] :: Map.Map String Int
ghci> :type ms2
ms2 :: Map.Map String Int


-- "Cheat Sheet" for the essential survival kit of `Data.List`, `Data.Set`, 
-- `Data.Map`

{-
    1. Data.List (The Bread & Butter)

    Standard lists `[]`. You don't usually need to import this, but some 
    specific helpers require `import Data.List`.


    ESSENTIAL FUNCTIONS
    - `nub`: Removes duplicates (O(n^2))
                ghci> nub [1, 1, 2, 3, 5, 8, 4, 4]
                [1,2,3,5,8,4]
    - `sort`: Sorts the list (requires `Ord`)
                ghci> sort [2,23, 2, 112, 1]
                [1,2,2,23,112]
    - `group`: Groups ADJACENT equal elements. (Tip: `sort` then `group` to
      group all duplicates).
                ghci> group [1, 1, 2, 3, 5, 8, 4, 4]
                [[1,1],[2],[3],[5],[8],[4,4]]

                ghci> (group . sort) [1, 1, 2, 3, 5, 8, 4, 4]
                [[1,1],[2],[3],[4,4],[5],[8]]

    - `permutations` / `subsequences`: Returns all orderings or subsets.
                ghci> permutations [1, 3, 2]
                [[1,3,2],[3,1,2],[2,3,1],[3,2,1],[2,1,3],[1,2,3]]

                ghci> subsequences [1, 3, 2]
                [[],[1],[3],[1,3],[2],[1,2],[3,2],[1,3,2]]

    - `transpose`: Swaps rows and columns (great for matrices) <-- tic tac toe match!

    - `intercalate`: Joins a list with a separator (like Python's `join`)
                ghci> :type subsequences
                subsequences :: [a] -> [[a]]

                ghci> intercalate [1, 3, 2] (subsequences [1, 2])
                [1,3,2,1,1,3,2,2,1,3,2,1,2]

                ghci> subsequences [1, 2]
                [[],[1],[2],[1,2]]


        ```Haskell
        intercalate ", " ["Hello", "World"]         -- "Hello, World"
        ```

    - `isInfixOf` / `isPrefixOf` / `isSuffixOf`: Checks if a sub-list exists 
      inside another.

            - `isPrefixOf needle haystack`  <-- True if `needle` is at the START of `haystack`
                    "is" `isPrefixOf` "island"   -- True
                    "is" `isPrefixOf` "this"     -- False
                    [1,2] `isPrefixOf` [1,2,3]   -- True
            - `isSuffixOf needle haystack`  <-- True if `needle` is at the end of the `haystack`
                    "hs" `isSuffixOf` "Main.hs"  -- True
                    "hs" `isSuffixOf` "haskell"  -- False
                    [3,4] `isSuffixOf` [1,2,3,4] -- True
            - `isInfixOf needle haystack`   <-- True if `needle` occurs ANYWHERE CONTIGUOUS inside `haystack` (substring/sublist).
                    "land" `isInfixOf` "island"  -- True
                    "and"  `isInfixOf` "island"  -- True
                    "isl"  `isInfixOf` "island"  -- True
                    "lsi"  `isInfixOf` "island"  -- False  -- not contiguous
                    [2,3]  `isInfixOf` [1,2,3,4] -- True



    - isPrefixOf        -- check if needle is at start of haystack              MATCH AT BEGINNING
    - isSuffixOf        -- check if needle is at the end of haystack            MATCH AT END
    - isInfixOf         -- check if needle is anywhere inside the haystack      MATCH ANYWHERE (prefix/suffix are special cases of infix)


    COMMON USE CASES
    - Filtering strings: keep lines that start with `"--" (comments), or contain `"TODO`
    - File/path checks: `isSuffixOf ".hs" filename` or whether `isPrefixOf "./src/" path`
    - Simple parsing/routing: detect command prefixes like `isPrefixOf "GET " requestLine`

    (They are case-sensitive, since they rely on `Eq`.)



    - `partition`: Splits a list in two based on a predicate.
                ghci> partition (== 1) [1..7]
                ([1],[2,3,4,5,6,7])
                ghci> partition odd [1..7]
                ([1,3,5,7],[2,4,6])

    - `find`: Returns the first element matching a predicate (returns `Maybe a`)
                ghci> :type find
                find :: Foldable t => (a -> Bool) -> t a -> Maybe a
                ghci> find (>3) [1..10]
                Just 4

    - `lookup`: Looks up a key in a list of tuples `[(k, v)]`
                ghci> :type lookup
                lookup :: Eq a => a -> [(a, b)] -> Maybe b
                ghci> lookup "A" [("A", 22), ("BC", 32)]
                Just 22




    ---

    2. `Data.Set` (Unique, Ordered Collection)    

    Import: `import qualified Data.Set as Set`
    Concept: A mathematical set. No duplicates, always sorted. Internally a
             balanced binary tree.


    CREATION & CONVERSION
        - `Set.fromList`: The most important one. List -> Set (removes dupes, 
          sorts).
        - `Set.toList`: Set -> List (returns sorted list).
                        ghci> import qualified Data.Set as Set

                        ghci> :type Set.fromList
                        Set.fromList :: Ord a => [a] -> Set a

                        ghci> :type Set.toList
                        Set.toList :: Set a -> [a]
        - `Set.empty` / `Set.singleton x`: Create empty or one-item set.


    CHECKS & QUERYING
        - `Set.member x s`: Is `x` in the set? (Returns Bool)
                        ghci> :type Set.member
                        Set.member :: Ord a => a -> Set a -> Bool
        - `Set.notMember`: The opposite
        - `Set.size`: Number of elements


    OPERATIONS (MATH LOGIC)
        - `Set.union` (A U B): Elements in A or B
        - `Set.intersection` (A \cap B): Elements in A and B.
        - `Set.difference` (A \ B): Elements in A but not B
 
                    ghci> :type Set.union
                    Set.union :: Ord a => Set a -> Set a -> Set a

                    ghci> :type Set.intersection
                    Set.intersection :: Ord a => Set a -> Set a -> Set a

                    ghci> :type Set.difference
                    Set.difference :: Ord a => Set a -> Set a -> Set a

    MODIFICATIONS
        - `Set.insert x s`: Add `x` (no-op if already there)
        - `Set.delete x s`: Remove `x` (no-op if not there)
                    ghci> :type Set.insert
                    Set.insert :: Ord a => a -> Set a -> Set a

                    ghci> :type Set.delete
                    Set.delete :: Ord a => a -> Set a -> Set a

        

            ghci> let a = Set.fromList [1, 2, 3, 4]

            ghci> a
            fromList [1,2,3,4]                  <-- easy way to be able to spot whats in the inside of a set during rapid modifications

            ghci> print a
            fromList [1,2,3,4]


        - `Set.map` ... You can transform all elements (build a new set) using `map`.
        ```Haskell
        Set.map (*2) (Set.fromList [1, 2, 3])           -- {2, 4, 6}
        ```
                            ghci> import qualified Data.Set as Set
                            ghci> :type Set.map
                            Set.map :: Ord b => (a -> b) -> Set.Set a -> Set.Set b

            (Internally it may need to rebalance/rebuild to keep the set ordered.)






    ---

    SO WHAT'S THE PURPOSE OF SETS?

    Sets are useful because they give you:
    - UNIQUENESS: no duplicates by construction.
    - FAST MEMBERSHIP: `member x s` is typically O(log n) for `Data.Set`.
    - SET OPERATIONS: `union`, `intersection`, `difference` are clean and 
      efficient.
    - COMMON PATTERNS
        * deduping data (`Set` instead of `nub`)
        * "visited" tracking in graph/search algorithms
        * maintaining a dynamic collection of unique keys (tags, IDs, 
          permissions)s



-}


{-
------- -   --  -   -   --------    ------       __   __
      /  \./  \/\_
  __{^\_ _}_   )  }/^\
 /  /\_/^\._}_/  //  /
(  (__{(@)}\__}.//_/__A____A_______A________A________A_____A___A___A______
 \__/{/(_)\_}  )\\ \\---v-----V-----V---Y-----v----Y------v-----V-----v---
   (   (__)_)_/  )\ \>
    \__/     \__/\/\/
       \__,--'
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
-}



{-
    3. `Data.Map` (Key-Value Store)

    Import: `import qualified Data.Map as Map`
    Concept: A Dictionary. Keys must be unique and `Ord`.


    CREATION & CONVERSION
    - `Map.fromList`: `[(k, v)] -> Map k v`
                Map.fromList :: Ord k => [(k, a)] -> Map.Map k a                <-- can run `:info Map.Map` too!
    - `Map.toList`: Get back the list of pairs.
    - `Map.empty` / `Map.singleton k v`: basics
                ghci> Map.empty
                fromList []

                ghci> :type Map.empty
                Map.empty :: Map.Map k a
            -----------|->
                ghci> :type Map.singleton
                Map.singleton :: k -> a -> Map.Map k a

                ghci> Map.singleton "he he he haw" [3, 3]
                fromList [("he he he haw",[3,3])]


    ACCESS (CRUCIAL!)
    - `Map.lookup k m`: Returns `Just v` or `Nothing`. (Safe access)
                    ghci> :type lookup
                    lookup :: Eq a => a -> [(a, b)] -> Maybe b
                    ghci> :type Map.lookup
                    Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
    - `Map.member k m`: Checks if key exists (Bool).
    - `Map.findWithDefault default k m`: Returns value or `default` if missing.
        ```Haskell
        Map.findWithDefault 0 "Charlie" scores          -- Returns 0 if Charlie isn't in maps
        ```
-}


prettyPrint :: Board -> IO ()
prettyPrint b = do
    let myRows = rows b
    for myRows $ \i -> do
        let temp   = showCell i             -- cellStrings
        putStrLn intersperse ' ' temp       -- formattedLine
    

{-
       .--.                   .---.
   .---|__|           .-.     |~~~|
.--|===|--|_          |_|     |~~~|--.
|  |===|  |'\     .---!~|  .--|   |--|
|%%|   |  |.'\    |===| |--|%%|   |  |
|%%|   |  |\.'\   |   | |__|  |   |  |
|  |   |  | \  \  |===| |==|  |   |  |
|  |   |__|  \.'\ |   |_|__|  |~~~|__|
|  |===|--|   \.'\|===|~|--|%%|~~~|--|
^--^---'--^    `-'`---^-^--^--^---'--' 
-}


{-
    MODIFICATION
    - `Map.insert k v m`: Adds/Overwrites key `k` with value `v`.
    - `Map.delete k m`: Removes key `k`.
    - `Map.adjust f k m`: Updates value at `k` by applying function `f` (only
      if key exists).
    ```Haskell
    Map.adjust (+1) "Alice" scores      -- Increments Alice's score
    ```
                    ghci> :type Map.insert
                    Map.insert :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
                    ghci> :type Map.delete
                    Map.delete :: Ord k => k -> Map.Map k a -> Map.Map k a
                    ghci> :type Map.adjust
                    Map.adjust :: Ord k => (a -> a) -> k -> Map.Map k a -> Map.Map k a


    ADVANCED BUT ESSENTIAL
    - `Map.union`: merges two maps. (Left-biased: if keys exists in both, keeps
      the one from the first map).
    - `Map.unionWith`: Merges, but if a collision occurs, uses a function to
      combine them.
      ```Haskell
      -- Merges scores, summing points if player exists in both
      Map.unionWith (+) map1 map2
      ```

                    ghci> :type Map.union
                    Map.union :: Ord k => Map.Map k a -> Map.Map k a -> Map.Map k a
                    ghci> :type Map.unionWith
                    Map.unionWith
                    :: Ord k => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a

-}





{-
------- -   --  -   -   --------    ------       __   __
      /  \./  \/\_
  __{^\_ _}_   )  }/^\
 /  /\_/^\._}_/  //  /
(  (__{(@)}\__}.//_/__A____A_______A________A________A_____A___A___A______
 \__/{/(_)\_}  )\\ \\---v-----V-----V---Y-----v----Y------v-----V-----v---
   (   (__)_)_/  )\ \>
    \__/     \__/\/\/
       \__,--'
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
-}

{-
    `unionWith` keeps ALL KEYS FROM BOTH MAPS; `intersectionWith` keeps ONLY
    keys that appear in BOTH MAPS. In both cases, when a key is present in
    both inputs, you provide a function to combine the two values.

    ```Haskell
    unionWith        :: Ord k => (a -> a -> c) -> Map k a -> Map k a -> Map k a
    intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
    ```


            ghci> print ms
            fromList [("a",2),("b",3),("c",4)]
            ghci> print ms2
            fromList [("a",3),("ab",1),("b",5),("c",20)]
            ghci> Map.unionWith (+) ms ms2
            fromList [("a",5),("ab",1),("b",8),("c",24)]
-}




{-
    ... a subtle constraint that dictates how these functions must be typed.

    The difference comes down to WHAT HAPPENS TO THE "LONERS" (keys that appear
    in only one of the two maps).

    Here is the logic in a nutshell:
    - UNION keeps the loners. (So everything must match).
    - INTERSECTION discards the loners. (So you have total freedom).

    Let's break it down.

    ---


    1. WHY `unionWith` forces `(a -> a -> a)`

        Imagine you are trying to union with two maps:
        - `Map 1`: `[(Key "A", Int 1)]` (Type: `Map String Int`)
        - `Map 2`: `[(Key "B", Bool True)]`


        If you try to union these, what is the type of the result?
        - It must contain Key "A" (value `1` :: Int)
        - It must contain Key "B" (value `True` :: Bool)

        CRASH: A Haskell Map cannot hold `Int` in one slot and `Bool` in another
        . A `Map k v` must have uniform values.                                     

        THE RULE: Because `union` preserves keys that exist in only one map, 
        both input maps must already have the same type (`Map k a`). Therefore,
        the combining function only ever sees two things of the same type:
        `f :: a -> a -> a`

    ---








    ---

    KEY DIFFERENCE IN BEHAVIOR


    COMMON USE CASES
    - 
-}
















{-
    SUMMARY CHEAT SHEET

    Operation               List (`Data.List`)              Set (`Data.Set`)                Map (`Data.Maps`)
    Convert from List       `id`                            `Set.fromList`                  `Map.fromList`
    Check exists            `elem`                          `Set.member`                    `Map.member`
    Access Item             `!!` (index), `lookup` (key)    N/A                             `Map.lookup`
    Insert                  `(++)`, `(:)` (cons)            `Set.insert`                    `Map.insert`
    Delete                  `List.delete`                   `Set.delete`                    `Map.delete`
    Filter                  `filter`                        `Set.filter`                    `Map.filter`
    Map (<$>)               `map`                           `Set.map`                       `Map.map`

        -- Map.keysSet
        -- Set.toList, Set.union


            -- `filter (/= x) xs` is recommended over delete for all occurrences
            -- use `Data.Set` of frequent deletes or frequent membership checks



        -- `length`

    CCA IDF M <-- convert from list // check exists // access item // insert // delete // filter // map
-} 