import Control.Monad
import Control.Applicative

import Data.Functor

{-
        "as the return type is just `m b` instead of `m m b`"

    YES! THIS IS THE DEFINING CHARACTERISTIC OF A MONAD.

    You asked if "concat applies more towards a definition type where we use
    list comprehension."

    You are 100% Correct.

    - `concat` is simply the name for "flattening" when you are working with 
      LISTS.
    - Every Monad has its own version of "flattening" (mathemaitcally called
      `join`) that collapses two layers `(m (m b))` into one `(m b)`.


    1. THE "STEP DOWN" (FLATTENING)

    You noticed that `>>=` essentially does `map` (which creates nested layers)
    and then "Steps down" (removes the nesting).

    There is actually a function in Haskell called `join` that does exactly 
    this:

    ```Haskell
    join :: Monad m => m (m a) -> m a
    ```

    The definition of `(>>=)` can actually be written as:

    ```Haskell
    (>>=) :: Monad m => m a -> (a -> m b) -> m b
    x >>= f = join (fmap f x)
    ```

        1. `fmap f x`: Runs the function `f` on the input. Since `f` returns a
           wrapped value (`m b`),
        2. `join`: Smashes the two layers together to get just `m b`.



    ---

    2. HOW DIFFERENT MONADS "Concat"

    You asked if `concat` is used regardless. NO. `concat` is specific to Lists.
    Other monads have different strategies for smashing layers.

    THE LIST STRATEGY (`concat`)
    - THE PROBLEM: You have `[[1, 2], [3, 4]]` (List of Lists)
    - THE LOGIC: "I represent multiple choices. Nested lists mean choices within
      choices. I want to flatten them into one big list of choices."
    - THE SOLUTION: `concat` --> `[1, 2, 3, 4]`


    THE MAYBE STRATEGY
    - THE PROBLEM: You have `Just (Just 5)` or `Just (Nothing)`
    - THE LOGIC: "I represent success or failure. Two layers of success is just
      success. Any failure at any layer is a total failure."
    - THE SOLUTION:
        * `join (Just (Just x)) = Just x`                                           <-- `import Control.Monad` needed
        * `join (Just Nothing) = Nothing`
        * `join Nothing = Nothing`



    THE PARSER/STATE STRATEGY (Run Sequence)

    This is the one relevant to your file.
    - THE PROBLEM: You have `Parser (Parser a)`. This is a "Parser that, when 
      run, produces another Parser."
    - THE LOGIC: "I don't want a parser that produces a parser. I want to RUN
      the first one, get the second one, and then RUN that one too immediately."
    - THE SOLUTION: Run the outer, take the result (inner), run the inner.



    3. WHY YOUR PARSER USES `concat`

    Your `Parser` is a hybrid. Is is a STATE MONAD (passing the string `s`) 
    logic wrapped around a LIST MONAD (returning `[(a, s)]`) result.

    Because your underlying result type is a LIST `[...]`, you must use `concat`
    to merge the results of the ambiguity.

    If you wrote a Parser that didn't support backtracking (it only returned 
    `(a, String)`) instead of `[(a, String)]` ), your `>>=` would not use 
    `concat`. It would look like this:
-}

-- A Parser that CANNOT fail or branch (Simple State)
newtype SimpleParser a = SP (String -> (a, String))             -- SP { runSimpleParser :: String -> (a, String) }   

instance Functor SimpleParser where
  fmap :: (a -> b) -> SimpleParser a -> SimpleParser b          -- px :: String -> (a, String)
  fmap f (SP px) = SP $ \s ->
    let (x, s') = px s
    in  (f x, s')

instance Applicative SimpleParser where
  pure :: a -> SimpleParser a
  pure x = SP $ \s -> (x, s)                          --  CORRECT: wrap value, pass string unchanged              //  WRONG: "SimpleParser" isn't the constructor... and wrong structure

  (<*>) :: SimpleParser (a -> b) -> SimpleParser a -> SimpleParser b
  (SP pf) <*> (SP px) = SP $ \s ->                --   now, once again, as practice ofc, we need to rmb that we want to return (b, String)
    let (f, s') = pf s          -- 1. run function parser, extract f
        (x, s'') = px s'        -- 2. run value parser on REMAINDER s'
    in  (f x, s'')              -- 3. apply f to x, return with final remainder
                                        -- same as list comprehension version... same threading logic... but just using `let` instead of list-comprehension.


instance Monad SimpleParser where
  (>>=) :: SimpleParser a -> (a -> SimpleParser b) -> SimpleParser b
  (SP p) >>= f = SP $ \s -> 
    let (x, s') = p s
        (SP q) = f x        -- (f x) :: SimpleParser b // CRITICAL STEP: Pattern match needed to get function 'q'
    in  q s'                -- <--- WHAT we actually want here is just the output `(x', s'') :: (a, String)`! This is because our definition 
                            --      here considers the wiring of `SP $ \s -> ...`
                            -- <--- ALTERNATIVELY: `result = runSimpleParser (f x) s'


newtype SimpleParser' a = SP' { runSimpleParser :: String -> (a, String) }



{-
    See? No `concat`. Just direct execution.


    SUMMARY
    - IS IT ALWAYS `concat`? No. `concat` is the implementation of `join` 
      specifically for Lists.
    - IS IT ALWAYS `join`? Yes. Every Monad must have a way to squash `m (m b)`
      into `m b`.
    - YOUR INSIGHT: You correctly identified that `(>>=)` is basically `map` +
      `flatten`.

                            (>>=)       ===           map <*> flatten
-}



{-
                    "M,        .mM"
                     IMIm    ,mIM"
                     ,MI:"IM,mIMm
          "IMmm,    ,IM::::IM::IM,          ,m"
             "IMMIMMIMm::IM:::::IM""==mm ,mIM"
    __      ,mIM::::::MIM:::::::IM::::mIMIM"
 ,mMIMIMIIMIMM::::::::mM::::::::IMIMIMIMMM"
IMM:::::::::IMM::::::M::::::::IIM:::::::MM,
 "IMM::::::::::MM:::M:::::::IM:::::::::::IM,
    "IMm::::::::IMMM:::::::IM:::::::::::::IM,
      "Mm:::::::::IM::::::MM::::::::::::::::IM,
       IM:::::::::IM::::::MM::::::::::::::::::IM,
        MM::::::::IM:::::::IM::::::::::::::::::IM
        "IM::::::::IM:::::::IM:::::::::::::::::IM;.
         "IM::::::::MM::::::::IM::::::::::mmmIMMMMMMMm,.
           IM::::::::IM:::::::IM::::mIMIMM"""". .. "IMMMM
           "IM::::::::IM::::::mIMIMM"". . . . . .,mM"   "M
            IMm:::::::IM::::IIMM" . . . . . ..,mMM"
            "IMMIMIMMIMM::IMM" . . . ._.,mMMMMM"
             ,IM". . ."IMIM". . . .,mMMMMMMMM"
           ,IM . . . .,IMM". . . ,mMMMMMMMMM"
          IM. . . .,mIIMM,. . ..mMMMMMMMMMM"
         ,M"..,mIMMIMMIMMIMmmmMMMMMMMMMMMM"
         IM.,IMI"""        ""IIMMMMMMMMMMM
        ;IMIM"                  ""IMMMMMMM
        ""                         "IMMMMM
                                     "IMMM
                                      "IMM,
                                       "IMM
                                        "MM,
                                         IMM,              ______   __
                        ______           "IMM__        .mIMMIMMIMMIMMIMM,
                   .,mIMMIMMIMM, ,mIMM,   IMM"""     ,mIM". . . . "IM,..M,
                 ,IMMM' . . . "IMM.\ "M,  IMM      ,IM". . . .  / :;IM \ M,
               .mIM' . . .  / .:"IM.\ MM  "MM,    ,M". . .  / .;mIMIMIM,\ M
              ,IM'. . .  / . .:;,IMIMIMMM  IMM   ,M". .  / .:mIM"'   "IM,:M
             ,IM'. . . / . .:;,mIM"  `"IMM IMM   IM. .  / .mM"         "IMI
            ,IM . .  / . .:;,mIM"      "IMMMMM   MM,.  / ,mM            "M'
            IM'. .  / . .;,mIM"          "IIMMM ,IMIM,.,IM"
            IM . . / . .,mIM"              IMMMMMMM' """
            `IM,.  / ;,mIM"                 IIMMM
             "IMI, /,mIM"                 __IMMM
               "IMMMM"                   """IMM
                 ""                         IMM
                                            IMM__
                                            IMM"""
                                            IMM
                                            IMM
                                          __IMM
                                         """IMM
                                            IMM
                                            IMM
                                            IMM__
                                            IMM"""
                                            IMM

------------------------------------------------
-}

{-
    The STATE MONAD is the logical evolution of the Reader Monad.
    - READER MONAD: You have a "Read-Only" global environment. (You pass 
      `config` down).
    - STATE MONAD: You have a "Read-Write" global environment. (You pass `state`
      down, and pass the MODIFIED `state` to the next guy).



-}

newtype Reader r a = RP { runReader :: r -> a}                      -- runReader :: (Reader r a) -> r -> a


instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (RP ra) = RP $ \r -> f (ra r)                      -- or simply: `Reader $ \r -> f (ra r)`


instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = RP $ \_ -> x             -- ignores environment! returns x, always!

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b            -- rf :: r -> (a -> b)          // \r === \inp
  (RP rf) <*> (RP ra) = RP $ \r ->                                  -- ra :: r -> a
    let f = (rf r)              -- get function from environment
        a = (ra r)              -- get value from SAME environment
    in  f a


instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (RP ra) >>= f = RP $ \r ->
    let a           = ra r
        (RP rb) = f a               -- rb :: r -> b
    in  rb r                            -- INTERESTINGLY! The input \r is always the same throughout! <-- which makes sense when you think about
                                        -- it. Because, `r` itself is not changing throughout! Because your still reader off the same functor of `* -> *`
                                        -- which of course in this case is a function... Think of this as like a config-processing-function or an 
                                        -- encryptor function! Which is used in all the same way throughout!

  



{-
    YES! The TYPE itself forces this:

    ```Haskell
    newtype Reader r a = Reader (r -> a)
    --                           ^    ^
    --                           |    output (no r here!)
    --                           input

    newtype State s a = State (s -> (a, s))
    --                        ^     ^  ^
    --                        |     |  output state (CAN thread!)
    --                        |     value
    --                        input state
    ```


    Reader has no `r` in the output--there's nothing to thread!
    ```Haskell
    
    -- State: s comes out, so you CAN pass new state forward
    (x, s') = p s
    q s'                -- pass s' (the NEW state)

    -- Reader: only `a` comes out, no new `r`
    x = ra r
    rb ???              -- what else can we pass? only have r!

    ```




    ---

    Practical example--Config:
-}
data AppConfig = AC { dbHost :: String, dbPort :: Int, apiKey :: String }           -- <-- this is writier monad?... odd. lol


-- Different functions all need config, but don't modify it
connectDB :: Reader AppConfig Connection
connectDB = Reader $ \config ->
  connect (dbHost config) (dbPort config)


callAPI :: Reader AppConfig Response
callAPI = Reader $ \config ->
  httpGet ("https:/api.com?key=" ++ apiKey config)


-- Compose them - both see SAME config
initApp :: Reader AppConfig (Connection, Response)
initApp = do
  conn <- connectDB         -- reads config
  resp <- callAPI           -- reads SAME config
  return (conn, resp)


-- Run with concrete config
main = runReader initApp (AppConfig "localhost" 5432 "secret123")





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