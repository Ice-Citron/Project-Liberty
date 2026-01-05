
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

