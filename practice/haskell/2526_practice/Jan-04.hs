import Control.Monad.State

{-
  The STATE MONAD is the logical evolution of the Reader Monad.
  - READER MONAD: You have a "Read-Only" global environment. (You pass `config`
    down).
  - STATE MONAD: You have a "Read-Write" global environment. (You pass `state`
    down, and pass the MODIFIED `state` to the next guy).

    It is the standard way to handle "mutable variables" (like game scores, 
    memory or random number seeds) in a purely functional language.



    1. THE CORE IDEA: THE "STATE PROCESSOR"

        In pure functional programming, you cannot change a variable `x = x + 1`
        . Instead, you must create a NEW version of the world.


    THE "MANUAL" WAY (THE PAINFUL WAY): 

    ```Haskell
    -- We return a Tuple: (The Answer, The New State)
    addPoints :: Int -> GameState -> (String, GameState)
    addPoints points oldState =
      let newState = oldState + points
      in  ("Points added!", newState)
    ```


    THE STATE MONAD TYPE: Just like Reader, we wrap this function pattern in
    a `newtype`.


    ```Haskell
    -- s = The type of the State (e.g., Int for a score)
    -- a = The type of the Return Value
    newtype State s a = State { runState :: s -> (a, s) }           -- runState :: State s a -> s -> (a, s)
    ```

    THE DIFFRENCE FROM READER:
    - Reader: `r -> a` (Produces a value).
    - State: `s -> (a, s)` (Produces a value AND a new state).



    ---

    2. THE PLUMBING: HOW `(>>=)` WORKS

    The State Monad's job is to handle the THREADING. It ensures that the 
    `newState` returned by Step 1 becomes the `oldState` for Step 2.


    The Logic of Bind (`>>=`):
    ```Haskell
    -- pseudo-code implementation of (>>=)
    (>>=) :: State a -> (a -> State b) -> State b
    (State sa) >>= (State sf) = State $ \initialState ->
      let (State sb) = runState sa initialState

            -- DECIDED TO SKIP FOR NOW BECAUSE I ALREADY KNOW THIS!
    ```



    ----

    This is the magic. You write code that looks like `step1 >>= step2`, and the
    Monad secretly pipes `state2` from the first line to the second line.



    -=-=-=-=-

    3. THE TOOLSET: Get, Put, Modify

    You interact with the hidden state using three primitive commands.

        1. `get`: "Copy the current state into the value slot so I can look at 
           it."
        2. `put`: "Ignore the current state; repalce it with this new one."
        3. `modify`: "Take the current state, run a function on it, and save the 
            result."



    4. EXAMPLE: A STACK MANIPULATOR

    Let's model a Stack (a list of Ints) where we can push and pop values.


    WITHOUT MONAD (UGLY):
    ```Haskell
    stackStuff :: [Int] -> (Int, [Int])
    stackStuff stack =
      let (a, stack2) = pop stack
          (b, stack3) = pop stack2
          stack4      = push (a + b) stack3
      in  (a, stack4)       -- It's a mess of variables!
    ```



    WITH STATE MONAD (Clean):
    -- import Control.Monad.State
-}

type Stack = [Int]

-- Helper to pop
{-
pop :: State Stack Int
pop = do
    (x:xs) <- get       --  Read the current list
    put xs              -- Save the list WITHOUT the head (xs)
    return x            -- Return the head as the value
-}


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

        -- NOPE, once again, I don't get it... and unfortunately, we need to skip as we are already out of time here.


{-
    SUMMARY
    
    1. THE WRAPPER: `State s a` wraps a function `s -> (a, s)`.
    2. THE "HIDDEN VARIABLE": The state `s` is passed along the chain 
       automatically.
    3. THE DIFFERENCE: Unlike `Reader` (which passes the same config to 
       everyone), `State` passes the updated config to the next person.s
    4. USE CASE: Game loops, parsing (whee `s` is the remaining string), random
       number generation (where `s` is the seed).s
-}



------------    --------    --  --  --------    --- --- -   --  ------  -   ---

{-
         ____
        (____)
        /____\
        |___.-~-.-~-.
        |__(  __|__  )_
        |/ \/\_/^\._)/ \
        (  (__{(@)}\_)  )
        |\_/ (/(_)\_))_/
  ______|_(  (__)_)_/ )
 /_________\_/  |  \_/
|/   /' |\  /'-~'~-'\|
    |  (| \/ |
    |   `\   |
     `\  `\  |    ___
       `\  `\|  /' ..'>
      ___`\  `\: ,' /'
    /' _ /''`\  '__'
   < .'./'   |  |
    `~' |    |  |
        |    |/'
        |    |
        |    |
        |    |
         \  /
          \/
-}

------------    --------    --  --  --------    --- --- -   --  ------  -   ---


{-
THE TWO FACES OF MONAD: `join` and `(>>=)`

    `join`--flattening nested structures:

    ```Haskell
    join :: Monad m => m (m a) -> m a
    ```

    For Maybe:
    ```Haskell
    joinMaybe :: Maybe (Maybe a) -> Maybe a
    joinMaybe Nothing   = Nothing       -- no inner Maybe to extract
    joinMaybe (Just mx) = mx            -- unwrap and return the inner Maybe
    ```

    For Lists:
    ```Haskell
    joinList :: [[a]] -> [a]
    joinList = concat                   -- just flatten!
    ```


        .....

    ---


    `(>>=)` -- "bind", the workhorse

    ```Haskell
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    ```

    Read this as: "Take the value(s) out of the `m a`, apply the function each, 
    and flatten the results."


        For Maybe.
        ```Haskell
        Nothing >>= f = Nothing         -- nothing to apply f to
        Just x  >>= f = f x             -- apply f, which produces another Maybe
        ```

        For lists:
        ```Haskell
        xs >>= f = concatMap f xs       -- apply f to each element, concat results
        ```




        THE KEY INSIGHT: `(>>=)` and `join` are EQUALLY POWERFUL. You can define
        each in terms of the other:

        ```Haskell 
        mx >>= f    = join (fmap f mx)
        join mmx    = mmx >>= id
        ```
-}


------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---
            -- mx >>= f     = join (fmap f mx)
            -- join mmx     = mmx >>= id
{-
    WHAT IF SOMETHING FAILS?                                        

    ```Haskell
    safeDiv 10 0 >>= (\m ->
      safeDiv 6 3 >>= (\n ->
        safeDiv m n
      )    
    )
    ```

    Step 1: `safeDiv 10 0 = Nothing`

    ```Haskell
    Nothing >>= (\m -> ...) = Nothing
    ```

    We stop immediately! The `Nothing` propagates, and we never even try the
    second division. This is the "short-circuting" behavior
-}

------------    --------    --  --  --------    --- --- -   --  ------  -   ---



{-
    DO-NOTATION: MAKING IT READABLE.

    That nested lambda syntax is ugly. Haskell provides DO-NOTATION as syntactic
     sugar.

    ```Haskell
    safeDiv 10 2 >>= (\m ->
      safeDiv 6 3 >>= (\n ->
        safeDiv m n
      )
    )

    do
      m <- safeDiv 10 2
      n <- safeDiv 6 3
      safeDiv m n
    ```


    EACH `x -> action` desugars to `action >>= (\x -> ...)`.

    RULES FOR DO-NOTAION:
    1. Each line is an action in the monad
    2. `x <- action` binds thje result of `action` to `x`
    3. The last line must be an action (not a binding)
    4. IF YOU DON'T NEED THE RESULT, JUST WRITE THE ACTION ALONE





    x <- action     ===     action >>= (\x -> ...)

-}




------------    --------    --  --  --------    --- --- -   --  ------  -   ---


{-
    THE RELATIONSHIP: FUNCTOR < APPLICATIVE < MONAD


    Every Monad gives you Applicative for free:

    ```Haskell
    liftA2 f mx my = do
      x <- mx
      y <- my
      pure (f x y)

    -- ===                      OR EQUIVALENTLY:
    liftA2 f mx my = mx >>= (\x -> fmap (f x) my)                   -- so interestingly,  i believe unlike the do-notation variant
                                                                    -- this raw variant relies on `fmap :: (a -> b) -> f a -> f b`
                                                                    -- ... since it takes (f x) as `f :: a -> b -> c`
    ```

    WHEN TO USE WHICH:
    - FUNCTOR: Transform values, one structure at a time
    - APPLICATIVE: Combine strucutres, but the "shape" of computation is fixed
      upfront.
    - MONAD: The result of one computation determines what to do next
-}




------------    --------    --  --  --------    --- --- -   --  ------  -   ---
-- A CONCRETE EXAMPLE: BUSH (TREE)

data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving Show
{-
    `join` for BUSH--GRAFTING TREES:                                -- defintion of 'grafting' == insert (a shoot or twig) as a graft.

    Imagine a `Bush (Bush a)` -- a tree where each leaf contains another tree.
    `join` "grafts" those inner trees into place:
-}

joinBush :: Bush (Bush a) -> Bush a
joinBush (Leaf t) = t                                       -- replace tree wrapper with its contents
joinBush (Fork lt rt) = Fork (joinBush lt) (joinBush rt)    -- recurse


{-
**Visual example:**
```
Before (Bush (Bush Int)):

    Fork
   /    \
Leaf    Leaf
  |       |
 Fork   Leaf
 / \      |
1   2     3

After join:

    Fork
   /    \
 Fork   Leaf
 / \      |
1   2     3
-}


{-
    mx >>= f        ===         join (fmap f mx)                <-- i believe this is also contributed by the fact that fmap itself also outputs f container
    join mmx        ===         mmx >>= id

-}


------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---

-- `(>>=)` for BUSH -- SPROUTING:
instance Monad Bush where
  (>>=) :: Bush a -> (a -> Bush b) -> Bush b
  Leaf x     >>= f = f x                                -- replace leaf with f's result
  Fork lt rt >>= f = Fork (lt >>= f) (rt >>= f)         -- recurse


-- EXAMPLE: MAKING TREES GROW IN SPRING
sprout :: Int -> Bush Int
sprout n = Fork (Leaf n) (Leaf (n + 1))

spring :: Bush Int -> Bush Int
spring t = t >>= sprout

{-
Starting tree:
```
    Fork
   /    \
Leaf 1  Leaf 3
```

After `spring`:
```
      Fork
     /    \
   Fork   Fork
   / \    / \
  1   2  3   4
-}


-- Each leaf was replaced by a small sprout!


------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---

-- IO: THE REAL WORLD MONAD

{-
    `IO a` represents a PROGRAM that, when executed, interacts with the outside 
    world and produces a value of type `a`.

    KEY OPERATIONS:
                putStrLn  :: String -> IO ()           -- print a line
                print     :: Show a => a -> IO ()      -- print any showable value
                getLine   :: IO String                 -- read a line of input
                readFile  :: FilePath -> IO String     -- read a file
                writeFile :: FilePath -> String -> IO ()  -- write a file
-}


-- YOUR FIRST REAL PROGRAM:
main :: IO()
main = putStrLn "Hello World!"


-- SEQUENCING WITH DO-NOTATION:
greet :: IO ()
greet = do
  putStrLn "What's your name?"
  name <- getLine                   -- bind the result to 'name'
  putStrLn ("Hello, " ++ "!")


-- CONDITIONAL LOOPING:
askUntilYes :: IO ()
askUntilYes = do
  putStrLn "Do you like Haskell?"
  answer <- getLine
  if answer == "yes"
    then pure ()        -- stop
    else askUntilYes    -- recurse



{-
    THE KEY INSIGHT: We can use regular Haskell control flow (`if`, recursion)
    because each branch just produces an `IO ()` action.

    ---

    ### THE OPERATOR FAMILY: Visual Guide

    (<$>) :: (a -> b) -> t a -> t b         -- Functor: transform inside
    (<$) :: a -> t b -> t a                 -- Functor: replace with constant

    (<*>) :: t (a -> b) -> t a -> t b       -- Applicative: apply wrapped function
    (<*) :: t a -> t b -> t a               -- Applicative: do both, keep left
    (*>) :: t a -> t b -> t b               -- Applicative: do both, keep right

    (>>=) :: m a -> (a -> m b) -> m b       -- Monad: Chain with function
    (>>) :: m a -> m b -> m b               -- Monad: Chain, ignore first result


    ---


    THE ARROW POINTS TO WHAT YOU KEEP
    - `<*` keeps left
    - `*>` keeps right
    - `>>=` chains forward (and the `=` means "binds the result")

-}



------------    --------    --  --  --------    --- --- -   --  ------  -   ---

------------    --------    --  --  --------    --- --- -   --  ------  -   ---



------------    --------    --  --  --------    --- --- -   --  ------  -   ---
------------    --------    --  --  --------    --- --- -   --  ------  -   ---




------------    --------    --  --  --------    --- --- -   --  ------  -   ---


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
    
-}