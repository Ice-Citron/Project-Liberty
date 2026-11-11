import Data.List (nub, delete)      -- for finding free/global variables
import Data.List (lookup)           -- for alpha-equivalence

----------- --- ----            --- -   -   -   --------    -
-- 5. `scanl` for RUNNING ACCUMULATION


{-
    3. HOW DO MAPS WORK? (Internal Structure)

    You asked: "do maps allow for different data types?" NO. A standard Haskell
    `Data.Map` is strictly homogeneous.

    - `Map String Int` = Every value is an Int.
    - `Map String Any` = Doesn't exist (unless you create a wrapper type).

    UNDER THE HOOD: A `Map` in Haskell is usually implemented as a BALANCED
    BINARY SEARCH TREE (specifically a Size Balanced Binary Tree)/
-}

data Size k v m1 m2 = S {runSize :: k -> v -> m1 -> m2 -> [(k, v, m1, m2)]}

-- Simplified definition of the Map data type.
data Map' k v = Tip
              | Bin (Size k v (Map' k v) (Map' k v))            -- this is actually not the official definition! But
                                                                -- im just fucking around and merely just wanna get the 
                                                                -- compiler error shoo'd off. lol!


{-
    HOW IT WORKS

    1. SORTED KEYS: The tree relies on the keys `k` being orderable (`Ord`
       typeclass).
    2. LOOKUP: To find a key, it starts at the root. Is your key smaller? Go
       left. Larger? Go right. This makes lookup fast (O(log n)).
    3. RECURSION: Functions like `mapM`, `union`, or `intersection` just walk
       down this tree structure recursively, exactly like you did with your
       `Tree` monad earlier!

    SUMMARY
    - UNION: "I might return a value from Left, OR a value from Right, OR a mix.
      " --> Left and Right must be the same type.
    - INTERSECTION: "I will only return a mixed value." --> Left and Right can
      be totally different.
-}


-----   --- --  --  -   -   -   --  ----------- -   --------    --------------
{-
 \  /  \  /  /  \  /  /  \  \  /  /  \  /  \  / 
\ \/ /\ \/ // /\ \/ // /\ \\ \/ // /\ \/ /\ \/ /
 \  /  \  /  /  \  /  /  \  \  /  /  \  /  \  / 
 \  /  \  /  /  /  \  /  /  \  /  \  /  \  /  / 
\ \/ /\ \/ // // /\ \/ // /\ \/ /\ \/ /\ \/ // /
 \  /  \  /  /  /  \  /  /  \  /  \  /  \  /  / 
 /  \  /  \  /  \  /  \  /  /  \  /  /  \  /  \ 
/ /\ \/ /\ \/ /\ \/ /\ \/ // /\ \/ // /\ \/ /\ \
 /  \  /  \  /  \  /  \  /  /  \  /  /  \  /  \ 
 \  /  /  /  /  \  /  /  /  \  /  /  \  /  \  \ 
\ \/ // // // /\ \/ // // /\ \/ // /\ \/ /\ \\ \
 \  /  /  /  /  \  /  /  /  \  /  /  \  /  \  \ 
 /  \  /  \  /  /  \  /  /  \  /  \  \  \  /  \ 
/ /\ \/ /\ \/ // /\ \/ // /\ \/ /\ \\ \\ \/ /\ \
 /  \  /  \  /  /  \  /  /  \  /  \  \  \  /  \ 
 \  \  \  /  /  \  \  /  \  /  /  \  /  /  \  / 
\ \\ \\ \/ // /\ \\ \/ /\ \/ // /\ \/ // /\ \/ /
 \  \  \  /  /  \  \  /  \  /  /  \  /  /  \  / 
 /  \  /  \  /  /  /  /  \  /  /  \  /  \  \  / 
/ /\ \/ /\ \/ // // // /\ \/ // /\ \/ /\ \\ \/ /
 /  \  /  \  /  /  /  /  \  /  /  \  /  \  \  /
-}
-----   --- --  --  -   -   -   --  ----------- -   --------    --------------


{-
    you need to focus on PATTERNS and STANDARD IMPLEMENTATIONS. ... falls into
     specific "buckets" for Lambda Calculus.

    ---


    1. THE DATA STRUCTURE (THE "AST")

    Almost every coding question starts here. Memorize this type definition.
-}
type Id = String

data Expr = Var Id              -- Variable (e.g., x)
          | App Expr Expr       -- Application (e.g., f x)
          | Lam Id Expr         -- Abstraction (e.g., \x -> x)
          deriving (Show, Eq)



{-
    -------
    2. Finding Free Variables

    EXAM QUESTION: "Implement a function `freeVars` that returns a list of all
    free variables in an expression."

    You already have the core logic, but here is the polished "exam-ready" 
    version.
    - TRAP: Forgetting `nub`. If `x` appears twice freely, list it once.
-}




-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-

{-
    This is the single most common confusion when starting Lambda Calculus. You
    are looking for `+`, `-`, `*`, or numbers like `1`, `2`, `3` in the 
    defintion.

    THEY ARE NOT THERE.

    Pure Lambda Calculus is like a universe made entirely of "verbs" (actions).         <-- reminds me of the wiring of parsers!
    There are no "nouns" (numbers/data). If you want a number, you want to build
    it out of verbs.

    Here is the breakdown of the syntax using an IRL Math analogy.


    ---



    1. WHERE IS THE FUNCTION DEFINED? (`Lam`)

    In standard math, you write:

                    f (x) = x + 1

    In Lambda Calculus, we strip away the name "f" and just write the raw logic.

                    \x.x + 1

    In your Haskell Data Type, that is `Lam "x" (... body ...`
    - `Lam` (Lambda): This is the function definition. It says "I am a function.
      I take an input named `x`. Here is what I do with it."
    - `Var` (Variable): This is using the input.
    - `App` (Application): This is calling the function.



    The Mapping:

    - STANDARD CODE : `x`
    - HASKELL `Expr`: `Var "x"` 
    - MEANING       : "Use the value labelled x"

    - STANDARD CODE : `f(x)`
    - HASKELL `Expr`: `App (Var "f") (Var "x")`
    - MEANING       : "Run function f with input x"

    - STANDARD CODE : `\x -> body`
    - HASKELL `Expr`: `Lam "x" body`
    - MEANING       : "Define a function that takes x"


-}

vx = Var "x"                    -- use the value labelled x
fx = App (Var "f") (Var "x")    -- run function f with input x
lx = Lam "x" (App (App (Var "-") (Var "3")) (Var "x"))
-- lx = Lam "g" body               -- define a function that takes x


vx' = Var "x"                   -- Use the value labeled x  
fx' = App (Var "f") (Var "x")   -- Run function f with input x
lx' = Lam "x" (App (App (Var "-") (Var "3")) (Var "x"))         -- \x -> x - 3
-- lx' = Lam "g" body              -- Define a function that takes x



-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
{-
    2. "HOW DO YOU DO MATH?" (The Mind-Bending Part)

    You asked: "how do you get the ability to do custom math operations like 
    algebra"

    ANSWER: You don't get them for free. You have to invent them using only
    function calls.

    Imagine you are in a room with ZERO objects. No apples, no counters. How do 
    you represent the number "3"?


    You can represent "3" BY DOING AN ACTION 3 TIMES.
    - 0 = "Don't do anything"
    - 1 = "Apply the function `f` once"     <-- `Lam "f" body`          or          `Var "f"`
    - 2 = "Apply the function `f` twice"        (f $ f x)
    - 3 = "Apply the function `f` three times"  (f $ f $ f x)


    This is called CHURCH ENCODING. In your exams, you might see this@
    - ZERO: `\f x -> x`             (Ignore the action `f`, just return the input `x`)
    - ONE : `\f x -> f x`           (Apply `f` to `x` once)
    - TWO : `\f x -> f $ f x`       


    SO WHAT IS ADDITION (`+`)? Addition is jsut function composition. 
    "Add 2 and 3" literally means@ "Run the function 2 times, then run it 3 more
     times."

    (Note: In many Haskell exams, they allow you to cheat and add a `Num Int`
    constructor to `Expr` just to make questions easier. But pure Lambda 
    Calculus doesn't have it!)



    ---
    3. FINDING FREE VARIABLES (THE "SCOPE" ANALOGY)

    Now, let's look at `freeVars` with a solid analogy.

    Think of a Lambda `Lam "x" ...` as a closed Room.
    - The argument `"x"` is a local variable defined INSIDE that room.
    - Any other variable mentioned inside is a GLOBAL VARIABLE (Free Variable)
      that must come from outside.


    ANALOGY: You are writing a letter:
        "Dear x, please tell y that I love him."

        - `x`: You know who this is. It's the person receiving the letter. 
          (Bound variable).
        - `y`: Who is y? You didn't say. "y" must be someone known from the 
          context outside the letter. (Free variable)

    
    WALKING THROUGH THE CODE WITH THE ANALOGY:
-}




{-
      __...--~~~~~-._   _.-~~~~~--...__
    //               `V'               \\ 
   //                 |                 \\ 
  //__...--~~~~~~-._  |  _.-~~~~~~--...__\\ 
 //__.....----~~~~._\ | /_.~~~~----.....__\\
====================\\|//====================
                    `---`
-}

-- The Expression: \x -> x + y
-- In AST: Lam "x" (App (App (Var "+") (Var "x")) (Var "y"))

-- freeVars (Lam "x" body)

{-
    1. ENTER THE ROOM (`Lam "x"`): We enter a room where "x" is defined.
    2. CHECK THE BODY: We look for all names mentioned inside.
        - We find `"+"` (Free  - we don't know what plus is locally.)
        - We find `"x"` (Bound - we know who x is).
        - We find `"y"` (Free  - who is y?)
        - RAW LIST: `["+", "x", "y"]`
    3. EXIT THE ROOM: As we leave the room `Lam "x"`, we say: "Since `x` was
       defined in this room, it is no longer a mystery."
    4. FILTER: Remove "x" from the list.
    5. RESULT: `["+", "y"]`. These are the only mysteries left.
-}



{-
    ---

    4. ALPHA EQUIVALENCE (THE "RENAMING" RULE)

    ANALOGY:
        - Function A: `\money -> buy_candy money`
        - Function B: `\cash -> buy_candy cash`

        Are these functions different? NO. It doesn't matter if you call the 
        input "money" or "cash". The logic is identical. This is ALPHA 
        EQUIVALENCE.

        EXAM TIP: If the exam asks "Are `\x -> x` and `\y -> y` equivalent?", 
        say YES. If the exam asks "Are `\x -> y` and `\z -> y` equivalent?",
        say YES (Input name changed, but `y` is still the same free variable).



    ---
    5. SUBSTITUTION (The "Find and Replace" Danger)#

    This is the one that trips everyone up.

    TASK: `subst "x" "5" expression` (Replace every "x" with "5").

    
    SCENARIO 1: SIMPLE REPLACE
        Expr: `x + y`
        Result: `5 + y` (Easy).


    SCENARIO 2: THE SHADOWING TRAP (THE "HIDING" RULE)
        Expr: `\x -> x + 1`
        Task: Replace "x" with "5".

    - Wait! The `x` inside `\x -> ...` is a NEW local `x`. It's not the same as
    the global `x` we are trying to replace. 
    - RULE: If you hit a `Lam` that defines the same name you are replacing, \
    STOP. Do not go inside. Result: `\x -> x + 1` (Unchanged)


    SCENARIO 3: THE CAPTURE TRAP (THE "ACCIDENTAL IDENTITY" RULE)
        Expr: `\y -> x`
        Task: Replace `x` with `y`. Naive Result: `\y -> y`
                -- `subst "x" "y" expression`


    CRITICAL FAILURE!
    - Original meaning: "A function that ignores input and returns the global"
    - New meaning: "A function that returns its own input `y`."
    - We changed the meaning of the program! The global `y` got "captured" by
      the local `\y`.


    ---

    SUMMARY FOR YOUR CHEATSHEET
    1. `Lam`  = Function Definition.
    2. `App`  = Function Call.
    3. `Math` = Doesn't exist. We simulate it by running functions N times.
    4. FREE VARIABLES     = A variable used but not defined in the current room.
    5. ALPHA EQUIVALENCES = Variable names don't matter (`\x -> x` == `\y -> y`)
    6. SUBSTITUTION       = Find and Replace, but STOP if you hit a Shadow, and 
                            RENAME if you risk Capture.




-}
-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-- LAMBDA CALCULUS


{-
    ... focus on PATTERNS AND STANDARD IMPLEMENTATIONS. 

    
    1. THE DATA STRUCTURE (THE "AST")

    Almost every coding question starts here. Memorize this type definition.

    ```Haskell
    type Id = String
    
    data Expr = Val String                  -- Variable (e.g., x)
              | App Expr Expr               -- Application (e.g., f x)
              | Lam Id Expr                 -- Abstraction (e.g., \x -> Expr)       <-- Expr can be very complicated!! beecause of `App` and `Val` can be anything as well
              deriving (Show, Eq)
    ```

    ---


    2. FINDING FREE VARIABLES

    EXAM QUESTION: "IMPLEMENT A FUNCTION `freeVars` that returns a list of all 
    free variables in an expression."

            -- MORE DETAIL
                `freeVars` computes the set of variable names that occur free in a lambda-calculus expression
                --o.e., occurrences that are not bound by any surrounding `Lam`.

                WHAT "free" means
                - In `\x -> ...`, the variable `x` is BOUND inside the body.
                - Any variable occurrence that isn't bound by some enclosing `Lam` is free.

    You already have the core logic, but here is the polished "exam-ready" 
    version.

-}

-- import Data.List (nub, delete)
freeVars :: Expr -> [Id]
freeVars (Var x)         = [x]
freeVars (App lexp rexp) = nub (freeVars lexp ++ freeVars rexp)
freeVars (Lam x   exp)   = delete x (freeVars exp)
        -- Note: `delete x` removes the first occurrence.
        -- Better to use `filter (/= x)` to be safe against duplicates if you didn't nub the child.


{-
                - `nub xs` removes ALL DUPLICATES (keeping first occurrences).
                - `delete x` removes ONLY THE FIRST occurrence of `x` from its input list.

                So `delete x (nub xs)` removes `x` (at most once, but after `nub` 
                there's at most one anyway) and ALSO DEDUPLICATES EVERY OTHER ELEMENT.

                Whereas `filter (/= x) xs` removes all `x`s but KEEPS DUPLICATES of
                everything else.


                ---

                Counter-example:
-}
ex  = 2
exs = [1, 2, 1]

exx1 = delete ex (nub exs)        -- nub xs = [1, 2]          -> delete 2 [1, 2] = 1
exx2 = filter (/= ex) exs         -- [1, 1]

        -- They are equal only under a condition like: `xs` HAS NO DUPLICATES AMONG ELEMENTS
        -- OTHER THAN `x` (dplicates of `x` don't matter because the filter removes them all).


        -- nub :: Eq a => [a] -> [a]
        -- delete :: Eq a => a -> [a] -> [a]


{-
  |
  |
  + \
  \\.G_.*=.
   `(#'/.\|
    .>' (_--.
 _=/d   ,^\
~~ \)-'   '
   / |   a:f
  '  '
-}


{-
    3. ALPHA EQUIVALENCE (alpha-conversion)


    EXAM QUESTION: "Implement `alphaEq` to check if two expressions are 
    effectively the same, ignoring variable names." or "Are `\x -> x` and
    `\y -> y` alpha-equivalent?"


    CONCEPT: `\x -> x` is the same function as `\y -> y`. They are 
    alpha-equivalent. 

    THE TRICK: You cannot just compare the strings. You usually solve this by
    renaming variables to a standard index (0, 1, 2...) or by maintaining a
    mapping of "variable `x` in Left equals variable `y` in Right."

    SIMPLEST EXAM APPROACH (Renaming): Rename all bound variables to a fresh
    stream of names (v1, v2, v3...) as you descend.
-}


-- Conceptual check (Manual trace)
-- Q: Are (\x -> x z) and (\y -> y z) alpha equivalent?
-- A: Yes
-- Q: Are (\x -> x z) and (\y -> y x) alpha equivalent?
-- A: No. The 'z' is free in the first, 'x' is free in the second.


{-      <-- WRITING OUT FOR REFERENCE!
import Data.List (lookup)                       lookup :: Eq a => a -> [(a, b)] -> Maybe b

type Id = String

data Expr = Var Id
          | App Expr Expr
          | Lam Id Expr
          deriving (Show, Eq)
-}

-- | The Main Function
alphaEq :: Expr -> Expr -> Bool
alphaEq exp1 exp2 = (normalize exp1 == normalize exp2)


-- | The Helper: Converts an expression to "Canonical Form"
-- It renames bound variables to v0, v1, v2... strictly based on depth.
normalize :: Expr -> Expr
normalize e = go [] 0 e
  where
    -- Case 1: Variables              go :: [(OldName, NewID)] -> NextID -> Expr -> Expr   
    go :: [(Id, Id)] -> Int -> Expr -> Expr         -- ah wow, i just realised... this form also works too. because of the fact that Lam only allows for 1 input!   //      which actually is fine... because lambda calculus only allows 1 input argument at a time!!
    go env _ (Var x) = case lookup x env of
        Just id -> Var ("v" ++ id)         -- If bound, replace with canonical ID
        Nothing -> Var x                   -- If free, LEAVE IT ALONE (Cruicial!)

    -- Case 2: Applications (Just recurse)
    go env count (App lexp rexp) = App (go env count lexp) (go env count rexp)

    -- Case 3: Lambdas (The Logic)
    go env count (Lam x body) = 
      let newName = "v" ++ (show count)                 -- Generate fresh name (v0, v1 ...)
          newEnv  = (x, show count) : env                    -- Map old 'x' to current ID
      in  Lam newName (go newEnv (count + 1) body)      


{-
           ;               ,           
         ,;                 '.         
        ;:                   :;        
       ::                     ::       
       ::                     ::       
       ':                     :        
        :.                    :        
     ;' ::                   ::  '     
    .'  ';                   ;'  '.    
   ::    :;                 ;:    ::   
   ;      :;.             ,;:     ::   
   :;      :;:           ,;"      ::   
   ::.      ':;  ..,.;  ;:'     ,.;:   
    "'"...   '::,::::: ;:   .;.;""'    
        '"""....;:::::;,;.;"""         
    .:::.....'"':::::::'",...;::::;.   
   ;:' '""'"";.,;:::::;.'""""""  ':;   
  ::'         ;::;:::;::..         :;  
 ::         ,;:::::::::::;:..       :: 
 ;'     ,;;:;::::::::::::::;";..    ':.
::     ;:"  ::::::"""'::::::  ":     ::
 :.    ::   ::::::;  :::::::   :     ; 
  ;    ::   :::::::  :::::::   :    ;  
   '   ::   ::::::....:::::'  ,:   '   
    '  ::    :::::::::::::"   ::       
       ::     ':::::::::"'    ::       
       ':       """""""'      ::       
        ::                   ;:        
        ':;                 ;:"        
-hrr-     ';              ,;'          
            "'           '"            
-}
-- 4. Substitution (The "Big Boss" Question)
{-
    EXAM QUESTIONS: "Implement `subst :: Id -> Expr -> Expr -> Expr`" which
    performs `e[x := val]`."

    This is the hardest standard question because of VARIABLE CAPTURE.
    - GOAL: Replace free occurrences of `x` with `val`.
    - THE TRAP: If you substitute into a Lambda, you might accidentally bind a
      variable that should be free.


    ALGORITHM
    1. Var: If it matches target swap it. If not, keep it.
    2. App: Just recurse on both sides.
    3. Lam (The hard part):
    
        - Case 1 (Shadowing): `(\x -> x) [x := 5]`. The inner `x` shadows the 
          target. STOP. Don't replace inside.
        - Case 2 (Capture Hazard): `(\y -> x) [x := y]` If we naively swap `x`
          for `y`, we get `\y -> y`. We just capptured `y`!
            - SOLUTION: You must rename the lambda variable `y` to something
              fresh (like `z`), then substitute.

    
    EXAM-SAFE IMPLEMENTATION (Simplified): If the question allows, assume 
    "fresh names are handle" or implements a basic version that crashes on
    capture. But if they ask for capture-avoiding substitution, you need this 
    logic.
-}


subst :: Id -> Expr -> Expr -> Expr
subst x val (Var y)
  | x == y    = val
  | otherwise = Var y                                                           -- interestingly... like below... if you want to stop, or half the whole thing. just simply return the original base case value input!!!
subst x val (App lexp rexp) = App (subst x val lexp) (subst x val rexp)
subst x val (Lam y exp) 
  | x == y                = Lam y exp       -- Case 1: Shadowing. Stop.                 // `| x == y` hsa nothing to do with `freeVars val` or its length.
  | y `elem` freeVars val =                 -- Case 2: Capture risk             -- and tbh... what's very interesting here too is how val can not only be App... but it can also be a massive `App lexp rexp`!...               || capture risk....         see how (\y -> x)[x := y]   -->     (\y -> y) is bad! because the new y actually becomes something else...
      let y'   = y ++ "'"                   -- Generate fresh name with `'`
          exp' = subst y (Var y') exp       -- Rename body (exp) first... changing all y present to y'.
      in Lam y' (subst x val exp')                    -- Given that binder name of bounded-variable `y` is `val`. This means we need to first replace bounded-variable's name to primt in BOTH exp-BODY and NAME-input first s.t. capture risk won't happen later where bound shadows the replaced value in expr below!!
  | otherwise = Lam y (subst x val exp)       -- Case 3: SAFE! Recurse

{-
    `y `elem` freeVars val` is not checking whether "the body is inside val"; it checks
    whether the BINDER NAME (y) occures FREE in the expression you are about to insert.
-}


-----------

{-
import Data.List (nub, delete, union, lookup)

type Id = String

data Expr = Var Id
          | App Expr Expr
          | Lam Id Expr
          deriving (Show, Eq)

-- | 1. Helper: Find Free Variables (Required for Substitution
freeVars :: Expr -> [Id]
freeVars (Var x)         = [x]
freeVars (App lexp rexp) = nub $ (freeVars lexp) ++ (freeVar rexp)
freeVars (Lam x exp)     = filter (/= x) (freeVars exp)                 -- delete x (freeVars e) 
-}

-- | 2. The Big Boss: Capture-Avoiding Substitution
--   Usage: `subst x val expr == expr[x := val] `
subst' :: Id -> Expr -> Expr -> Expr
subst' x val (Var y)         
  | y == x    = val                                                             -- remember... during success case. we actually want to replace to val! because the end result is `expr[x := val]` 
  | otherwise = Var y                                                           -- if nothing happens, then return original
subst' x val (App lexp rexp) = App (subst' x val lexp) (subst' x val rexp)
subst' x val (Lam y exp)     
    -- check for shadowing... which is when (\x -> y)... gets turned into (\x -> x)... as y = x
  | x == y    = Lam y exp 
    -- check for capture...   which is when `y `elem` freeVars val` ... which means that there's val y (of global variable... which contradicts with local variable y) too inside freeVars... in this case we need to replace the Id in both local-variable and exp-BODY
  | y `elem` freeVars val = 
    let y'   = y ++ "'"
        exp' = subst' y (Var y') exp
    in  Lam y' (subst' x val exp')
    -- SAFE! Recurse ahead
  | otherwise = Lam y (subst' x val exp)




--------

{-
    5. BETA REDUCTION

    EXAM QUESTION: "Perform one step of reduction" or "Reduce to Normal Form".

    DEFINITION: `(\x -> body) arg` reduces to `body[x := arg]`.

    CODE PATTERN: You. usually need a helper to find the "redex" (reducible 
    expression).

    - NORMAL ORDER (Call-by-name): Reduce the outermost, leftmost lambda first.
    - APPLICATIVE ORDER (Call-by-value): Reduce the arguments first (inside-out).
-}

            -- Beta-reduction is FUNCTION APPLICATION in Lambda Calculus
reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var _) = Nothing
reduceOnce (Lam x e) = (Lam x) <$> (reduceOnce e)           -- fmap (Lam x) (reduceOnce e)
                                                            -- We generally don't reduce inside lambdas unless computing "full normal form"
                                                            -- If we strictly follow Normal Order, we might, but usually we stop at the lambda.
reduceOnce (App (Lam x body) arg) = Just (subst x arg body) -- The main event: Beta-reduction
reduceOnce (App lexp rexp) =
    case reduceOnce lexp of
      Just lexp' -> Just (App lexp' rexp)                   -- Try reducing the function spot
      Nothing    -> (App lexp) <$> (reduceOnce rexp)
      -- Nothing    -> fmap (App lexp) (reduceOnce rexp)       -- Else try reducing the argument


{-
    This is a crucial question. You are looking at the mechanics of FUNCTOR and
    CONSTRUCTORS colliding. Let's untangle `fmap` first, then look at the 
    "One Step" logic for `App`.


    1. WHAT ON EARTH IS `fmap (Lam x)`

    You are confused because you are thinking of `Lam` as a static data object. 
    In Haskell, CONSTRUCTORS are FUNCTIONS.



    THE TYPE LOGIC

    Look at the definition of your data type:
    ```Haskell
    data Expr = Lam Id Expr | ...
    ```


    This automatically defines a function named `Lam`: `Lam :: String -> Expr -> Expr`

    If we provide only the first argument (`x`), we get a PARTIAL APPLICATION:
    `Lam "x" :: Expr -> Expr`

    Now look at `reduceOnce`. It returns `Maybe Expr` (It might fail if there is
    no work to do).
        - INPUT: `Just (Expr)` (The reduced body)
        - FUNCTION: `Lam "x"` (The wrapper)
        - `fmap`: Applies the wrapper to the thing inside the `Just`.
-}

-- Scenario: reducing (\x -> 1 + 1)
-- 1. We look inside the body: (1 + 1)
-- 2. reduceOne (1 + 1) returns Just (2)
-- 3. We want to wrap it back up: Just (\x -> 2)

ffa = (Lam "x") <$> (Just (Var 2))          -- == Just (Lam "x" (Val 2))




{-
    WHY `:kind` FAILED

    You got an error with `:kind Lam "x"` because `Lam` IS A VALUE, NOT A TYPE.
        - KINDS (`:kind`) are for Types (like `Int`, `Maybe`, `Expr`).
        - TYPES (`:type`) are for Values (like `5`, `True`, `Lam "x"`, `map`)

    If you run `:type Lam "x"` in GHCi, it will tell you: `Lam "x" :: Expr -> Expr`



    "WHY ARE WE REDUCING INSIDE?"

    You asked: "I thought you said stop at lambda?"
-}



-------
-- 6. Common Church Encodings (Written Questions)





{-
    THE SEPARATION OF CHURCH AND STATE (Types vs. Values)
    ----

    `Expr` is a TYPE (a blueprint). It is not a VALUE (a concrete object).
    - Values exist at runtime (`5`, `"hello"`, `Lam "5" ...`)
    - Types exist at Compile Time (e.g. `Expr`, `Int`, `String`, etc.)


    `:type` asks: "What is the type of this value?"


    ------

    ERROR 2: `:type Lam String (Var String)`

    THE ERROR: `Illegal term-level use of the type constructor or class ‘String’`

    THE REASON: You are trying to build a value. The constructor `Lam` expects a
    STRING VALUE (text in quotes), but you gave it the TYPE NAME `String`.
    

```Haskell
-- WRONG (Passing Types)
Lam String (Var String)

-- CORRECT (Passing Values)
Lam "x" (Var "y")
```



                ghci> :info Expr
                type Expr :: *
                data Expr = Var Id | App Expr Expr | Lam Id Expr
                        -- Defined at Jan-06.hs:89:1
                instance Eq Expr -- Defined at Jan-06.hs:92:27
                instance Show Expr -- Defined at Jan-06.hs:92:21
                ghci> :kind Expr
                Expr :: *
                ghci> :type Lam "s" (Var "+")
                Lam "s" (Var "+") :: Expr
-}



-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-- SYMBOLIC CALCULUS





-- +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-


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