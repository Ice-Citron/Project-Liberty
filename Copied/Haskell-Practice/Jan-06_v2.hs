import qualified Data.Set as Set
import Data.Set (Set)                   -- It's a type! Can check it's info using `:info Set`

import Data.List (nub)

import qualified Data.Map as Map


-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

{-
If you are representing a graph as a list of edges `[(1, 2), (2, 3), (1, 3)]`,
you often need to extract the list of UNIQUE NODES.

    - THE EXAM QUESTION: "Given a list of edges, return the total number of
      unique nodes."
-}
type Edge = (Int, Int)

getNodes :: [Edge] -> [Int]
getNodes edges = nub $ [u | (u, v) <- edges] ++ [v | (u, v) <- edges]

        -- This is a classic use case because edges `(1, 2)` and `(2, 3)` both
        -- mention node `1`, so you need `nub` to count it only once.



{-
    1. THE WARM-UP: Extracting Nodes (Optimization Check)

    Your snippet uses `nub`, which is O(n^2).

    IMPERIAL EXAM TIP: If the question mentions "efficiency" or "large graphs,"
    `nub` loses marks. The "Pro" answer uses `Data.Set`.


    THE EFFICIENT PATTERN:
-}

getNodes' :: [Edge] -> [Int]         -- [Edge] :: [(Int, Int)]
getNodes' edges = Set.toList $ Set.fromList $ [u | (u, _) <- edges] ++ [v | (_, v) <- edges]
        -- Why? `Set.fromList` is O(n log n), which is much faster than `nub` for large lists.



{-
    2. THE TRANSFORMATION: EDGE LIST --> ADJACENCY MAP

    PROBABILITY: High.

    THE PROBLEM: A list of edges `[(1, 2), (2, 3)]` is bad for lookups. If I
    want to know "Who is connected to 1?", I have to search the whole list.

    THE TASK: Convert it to `Map Node [Neighbor]` so lookup is instant.

    THE "MAGIC" FUNCTION: `Map.fromListWith` This function is your best friend. 
    It takes a list of key-value pairs and saying: "If you find duplicate keys,
    combine their values using this function."
-}
-- import qualified Data.Map as Map
type Graph = Map.Map Int [Int]

-- Directed Graph Build
buildGraph :: [(Int, Int)] -> Graph                 -- Graph :: Map.Map Int [Int]
buildGraph edges = Map.fromListWith (++) [(u, [v]) | (u, v) <- edges]

-- Undirected Graph Build (Cruicial Variation!)
-- If 1 is connected to 2, then 2 must be connected to 1.
buildUndirected :: [(Int, Int)] -> Graph            -- Graph :: Map.Map Int [Int]
buildUndirected edges = Map.fromListWith (++) $ concat [[(u, [v]), (v, [u])] | (u, v) <- edges]




{-
----------------    --------    ----    --  -
3. THE TRAVERSAL: DEPTH FIRST SEARCH (Reachability)

    PROBABILITY: Very High.

    THE QUESTION: "Find all nodes reachable from start node `s`." or "Detect if
    there is a cycle."

    THE TRAP: Infinite Loops. If the graphs has a cycle `1 -> 2 -> 1`, a naive
    recursion will crash. You MUST keep track of `visited` nodes.

    THE ALGORITHM (THE "WORKLIST" APPROACH):
        - We treat the `visited` set as an accumulator.
-}
reachable :: Graph -> Int -> Set Int
reachable g start = dfs Set.empty [start]
  where
    -- visited: Set of nodes we have already seen
    -- stack  : List of nodes waiting to be processed
    dfs :: Set Int -> [Int] -> Set Int
    dfs visited [] = visited        -- Stack empty? We are done.

    dfs visited (node:rest)
      | Set.member node visited = dfs visited rest
      | otherwise =
          let
            -- 1. Find neighbours (Default to [] if node has no outgoing edges)
            neighbors = Map.findWithDefault [] node g

            -- 2. Mark current as visited
            newVisited = Set.insert node visited

            -- 3. Add neighbors to the stack to be processed
            newStack = neighbors ++ rest
          in
            dfs newVisited newStack



{-
    This is the critical distinction between ITERATING A DATABASE and TRAVERSING
    A GRAPH.
    
    You are absolutely right that if you just want to "list every node in the 
    graph," you can simply do `Map.keys graph`. That is safe, fast, and never
    loops.

    HOWEVER, GRAPH ALGORITHMS GENERALLY ASK ABOUT CONNECTIONS (REACHABILITY). 
    The question isn't "Who exists in the world?" (Iteration).
    The question is "Who can I REACH starting from home?" (Traversal).

    If you use your "Map.toList" method, you lose the connection data. You are
    just reading the phone book instead of tracing who calls whom.

    Here is the exact example you asked for, showing why the "Visited Set" is
    mandatory for traversal.


    ----
    The Lookup Table (Adjacency List)

    Imagine a graph with a loop: 1 AND 2 POINT TO EACH OTHER.
-}
-- Graph: 1 <--> 2 --> 3
graph :: Graph
graph = Map.fromList [
    (1, [2]),               -- Node 1 points to 2
    (2, [1, 3]),            -- Node 2 points back to 1, and also to 3
    (3, [])                 -- Node 3 is a dead end
]


-- THE GOAL
        -- "Find all nodes reachable starting from Node 1."

{-
    METHOD A: YOUR "NAIVE" RECURSION (The Infinite Loop)

    If we just blindly follow edges without a checklist (`Visited` set), here
    is the trace.

    LOGIC: "I am at Node X. Go visit all my neighbors."
        Start at 1.
        Who is my neighbor? 2.
        Action: Go visit 2.
        Now at 2.
        Who are my neighbors? 1 and 3.
        Action: Go visit 1.
        Now at 1. (Again!)
        Who is my neighbor? 2.
        Action: Go visit 2.
        Now at 2. (Again!)
        Action: Go visit 1.

    RESULT: Stack Overflow. The computer runs in circles forever until it 
    crashes. It never even gets to see Node 3 because it is stuck in the 1-2 
    loop.



    ------
    METHOD B: The "Visited Set" (The Cycle Breaker)

    - Now we add the rule: "CHECK THE LIST. IF I'VE SEEN YOU, I IGNORE YOU."

    State: `Visited = {}`, `Worklist = [1]`
        1. Pop 1.
            - Have I seen 1? No.
            - Mark 1 as Visited. `Visited = {1}`
            - Neighbors of 1 are `[2]`


    ...

    okay i get it... i will bear this in mind for the future... and just in case IF we do face this issue of infinite cycling.
-}


-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
{-
        ________________________
       / ____________________  /|
      / /|__________________/ / |
     / / | |               / / .|
    / / /| |              / / /||
   / / / | |             / / / ||
  / / /| | |            / / /| ||
 / /_/_| | |___________/ / /_| ||
/______| | /__________/_/ /__| ||
|  ____|_|/___________| | |____||
| | |  / / /          | | | / / /
| | | / / /           | | |/ / /
| | |/ / /            | | | / /
| | | / /             | | |/ /
| | |/ /              | | / /
| | |_/_______________| |  /
| |/__________________| | /
|_______________________|/  JEZ_mic
-}

-- .oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo..oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.
{-
    This is a classic "Compiler Pass" problem. In these types of questions, you 
    are essentially writing three distinct passes over an Abstract Syntax Tree
    (AST):

    1. ANALYSIS PASS (`freeVars` / `buildFVMap`): Gather information about the
       code.
    2. TRANSFORMATION PASS (`modifyFunctions`): Rewrite the code based on that
       information.
    3. EXTRACTION PASS (`lift`): Structural surgery to move pieces of code 
       around.

    Here is the walkthrough of the techniques required for each part.



    ---
    PART I: THE WARM UP (PATTERN MATCHING & LIST PROCESSING)

    These are standard recursive functions on the `Exp` data type.
-}

-- Part I

-- 1. Check if an expression is a function definition
isFun :: Exp -> Bool
isFun (Fun _ _) = True
isFun _         = False

-- 2. Split a list of bindings into (Functions, Variables)
--    Binding is type (Id, Exp). We need to check the Exp part (snd).
splitDefs :: [Binding] -> ([Binding], [Binding])
splitDefs [] = ([], [])
splitDefs (b:bs)
  | isFun (snd b) = (b : funs, vars)        -- It's a function, add to left
  | otherwise     = (funs, b : vars)        -- It's a variable, add to right
  where
    (funs, vars) = splitDefs bs
-- Alternative One-Liner (if Data.List.partition is allowed):
-- splitDefs = partition (isFun . snd)


-- 3. Count functions ONLY in the top-level Let
topLevelFunctions :: Exp -> Int
topLevelFunctions (Let bindings _) = length [b | b <- bindings, (isFun . snd) b]
topLevelFunctions _                = 0





-- Part 2
unionAll :: Eq a => [[a]] -> [a]
unionAll = nub . concat


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