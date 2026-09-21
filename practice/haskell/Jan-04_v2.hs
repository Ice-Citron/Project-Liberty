import Control.Monad.State
import Control.Monad (when)
import Control.Monad

{-
\\|//
  |
  :       \\|//
            |
            ;          ||/_
    \\|//               /--
      |                /
      ;       _\||
              --\      _\||
                 \     --\
                          \
                              " ' "
                            " \ | / "
                           ' --(:)-- '
                            " / | \ "
                             " '|' "
                          |\    |    /|
                          /_ \  |  / _\
                 ldb        /_ \|/_\ 
-}
{-
    EXAM INSIGHTS

    1. "What's the difference between Applicative and Monad?"

    With Applicative. the structure of your computation is fixed. You decide 
    upfront "I'll combine these three Maybes."

    With Monad, the result of one computation can determine what you do next:
-}

f :: IO ()
f = do 
    x <- getLine
    if x == "quit"
      then pure ()
      else print "stupid fucks"       -- depends on x!



{-
    2. "DESUGAR THIS DO-BLOCK"

do x <- mx          -- x <- mx      ===     mx >>= (\x = ...)
   y <- my
   pure (f x y)

-- becomes

mx >>= (\x -> my >>= \y -> pure (f x y))




    3. "WHEN COULD THIS HAVE BEEN APPLICATIVE INSTEAD?"

    If none of the bound variables are used to decide what action to take next,
    it could be Applicative:

do x <- mx
   y <- my
   pure (x + y)

-- ===

liftA2 (+) mx my

-- ===

(+) <$> mx <*> my
-}

------------    --------    --  --  --------    --- --- -   --  ------  -   ---
{-
  _  _  _
 {o}{o}{o}
  |  |  |
 \|/\|/\|/
[~~~~~~~~~]
 |~~~~~~~|
 |_______|
-}

{-
    4. "What does `join` do for type X?"

    It flattens nested structure. For `Maybe (Maybe a)`, it's "if both layers
    are Just, unwrap both." For `[[a]]`, it#s `concat`. For `Bush (Bush a)`,
    it's grafting inner trees into outer layers.


    5. "WRITE A MONAD INSTANCE FOR THIS TYPE"

    Think about `(>>=)`: you're extracting values from the first structure and
    using them to build new structures, then flattening. Pattern match on the
    constructors, recurse on recursive positions.


    6. QUICK PATTERN RECOGNITION:
        - See `concatMap`? That's `(>>=)` for lists.
        - See `concat`? That's `join` for lists.
        - See nested `case` on `Maybe`? Probably `(>>=)` or do-notation.
-}

------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---


{-
    The "weirdness" you feel is magic of MONADS. They build an abstraction layer
    that lets you write code that looks imperative (like Python) but remains 
    purely functional and mathematical underneath.

    And to answer your question: YES, YOU CAN ABSOLUTELY USE GUARDS INSIDE A 
    `case` STATEMENT WITHIN A `do` BLOCK/

    Because `do` notation is just syntactic sugar for chaining functions, 
    anything valid in standard Haskell (like `case` with guards) is valide          
    inside a `do` block.


            BECAUSE `DO` NOTATION IS JUST SYNTACTIC SUGAR FOR CHAINING FUNCTIONS,
            ANYTHING VALID IN STANDARD HASKELL (LIKE `CASE` WITH GUARDS) IS VALID               ANYTHING VALID
            INSIDE A `DO` BLOCK

    USING GUARDS IN `case`
-}

type Stack = [Int]

-- Helper to pop
pop :: State Stack Int
pop = do
  st <- get             -- 1. Get the whole stack (irrefutable)
  case st of
    (x:xs) -> do        -- 2. Pattern match manually
        put xs
        return x
    []     -> do        -- 3: Handles the empty case.
        error "Stack underflow: Cannot pop from an empty stack!"


-- Helper to push
push :: Int -> State Stack()
push x = do
    xs <- get           -- Read the current list
    put (x:xs)          -- Save the new list with x at the front


-- The Main Logic:
stackStuff :: State Stack Int
stackStuff = do
  a <- pop
  b <- pop
  push (a + b)
  return a

popEven :: State Stack Int
popEven = do
  st <- get
  case st of
    (x:xs) | even x -> do                   -- Guard: Only matches if list is non-empty AND head is even
        put xs
        return x
    (x:xs) -> do                            -- Fallback: What to do if it's odd?
        error "Top element is odd!"
    [] -> error "Stack is empty!"           -- Fallback: What to do if empty?


{-
    WHY THIS IS POWERFUL

    You are mixing three different layers of logic in one readable block:
    1. State Management: (`get`, `put`) handling the "global" variable.
    2. Structural Logic: (`case (x:xs)`) handling the shape of the data.
    3. Boolean Logic: (`| even x`) handling the specific values.


    A CLEANER ALTERNAITVE: `when` and `unless`

    If you find yourself writing simple `if` or guard logic inside `do` blocks
    often, Haskell provides helpers in `Control.Monad` like `when`.
-}

-- import Control.Monad (when)

checkAndLog :: State Stack ()
checkAndLog = do
    st <- get
    -- If stack is empty. print a warning (conceptually)
    -- Note: `trace` or similar debug usually needed here since State is pure
    when (null st) $ do
        -- This block only executes if st is empty
        return ()



------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---

{-
    You are effectively writing "imperative" scripts, but the compiler is
    secretly stitching them together into big, pure functions.

    ... replicate loops and list comprehensions using `do` notations


    ---

    1. "FOR LOOPS" (USING `forM_`)

    In Python, you do `for x in list: do_something(x)`. In Haskell, you use             `forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()`
    `forM_ ` (from `Data.Foldable`). It takes a list and a lambda (a mini 
    function) that acts as the loop body.

                    because of the fact that forM_ takes input (a -> m b)... which (a -> m b) is basically the same as the lambda 
                    function to the right of >>= and >> for monads... and given how these types of (a -> m b)... from what i see 
                    so far where these functions can in a way be like an imperative program and be as long as they want... so does 
                    this mean that forM_... like for in python... can basically contain any possible code?


-}



------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---



{-
    `forM_` can run an arbitrarily large "loop body", but only as a monadic 
    action--i.e., only whatever effects are permitted by the monad `m`.


    WHAT `forM_` ACTUALLY WANTS

        `forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m b`

    So the "loop body" is a function that, given an `a`, produces an action 
    `m b`.


    When you write
    ```Haskell
    forM_ xs $ \i -> do
        ...
    ```

    the `do ...` must have type `m something` (often `m ()`). It can be as long
    as you want, call helpers, do `let` bindings, pattern matches, etc.

                            “Any possible code” depends on m
                                    If m ~ IO, then yes: the body can do essentially any IO you want (print, read files, network, etc.).
                                    If m ~ State Stack, then the body can do state actions (push/pop/modify/get), but cannot do IO.
                                    If you want both, you use a transformer, e.g. StateT Stack IO, and then the body can do state + IO.


    
    ---

    EXAMPLE (pure state only):
-}

-- for1 :: StateT Stack Data.Functor.Identity.Identity ()
for1 = forM_ [1..5] $ \i -> do
    push i
    when (i == 3) (push 999)
    -- cannot `putStrLn` here if this is State Stacl


        {-
                    ghci> :t do push 5; when (5 == 3) (push 999)
                    do push 5; when (5 == 3) (push 999)
                    :: StateT Stack Data.Functor.Identity.Identity ()
                    ghci> :t do push 5; when (3 == 3) (push 999)
                    do push 5; when (3 == 3) (push 999)
                    :: StateT Stack Data.Functor.Identity.Identity ()
        -}

-- > execState for1 []



{-
    USE READER MONADS WHEN...
        - You need read-only configuration
        - Every computation sees the same env
        - Examples: config settings, DB connections

    USE STATE MONADS WHEN...
        - You need to modify something
        - Computations affect later computations
        - Examples: counters, caches, random generators


-}


------------    --------    --  --  --------    --- --- -   --  ------  -   ---

{-
SUMMARY OF FINDINGS
- one can use ghci's `:type` to check for the type of a `do` expression

                ghci> :t do x <- Just 3; y <- Just 4; pure (x + y)
                do x <- Just 3; y <- Just 4; pure (x + y) :: Maybe Int

                ghci> :t do x <- [1,2]; y <- [10,20]; pure (x+y)
                do x <- [1,2]; y <- [10,20]; pure (x+y) :: [Int]

---

    - By itself, `do` is a syntax, not a value... hence can't direclty use :t on it...
      akin to `(::)`

    - A `do` block is an expression whose type is some monadic type `m b`. It's 
      not "a function" unless you make it one... (e.g. `\x -> ...`)                     <-- do blocks are basically a chain of `(>>=)` and `(>>)`
                                                                                                                            nested binds... and sequencing

                                - `do` desugars into nested binds (>>=), sequencing ()>>) and lambdas (\x -> ...).








-}

------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---




{-
    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

       -- 2022 is hard
       -- 2025 seem to be about parsers... or state monads

                `nub`
                \case
                [ParseTree]
                split2 and split3.          <-- custom PPQ function... ig we will learn this more in ppq... but pay attention to function re-use...

    2. still don't understand the `commands` and `commands'` functions from bottom of ApplicativeParsing.lhs
            -- A potential concern rn tbf... is that we are very good at implementation of interfaces rn... but not much good at
               actually using these functions and HOFs... but this should be fixable in the next 3 days

    3. need lots of practice with writing `do`... oddly enough they seem to be similar to list-comprehensions... check ed-stem
            -- ask gemini to provide me examples... like to translate LCs to Do and Dos back to LC


    ADDITIONAL
        - Claude chat - Four Approaches to LSystem Parsing
  
-}