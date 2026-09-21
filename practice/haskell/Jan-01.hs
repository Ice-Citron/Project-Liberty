-- {-# LANGUAGE BangPatterns #-}                -- not needed because we are using GHC 2021
import Control.Applicative
import Data.Char (isDigit, isAlpha)

import Data.Functor

-- 1. THE TYPE DEFINITION (The "Reader Monad ish" part)
-- It wraps a function: String -> [(a, String)]
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- 2. BOILERPLATE INSTANCES (To make <|> and <*> work)
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \s -> [(f x, s') | (x, s') <- p s]     -- looks like by default (without `(<$>)` and `f` influence) it would've been `Parser p = Parser $ \s -> [(x, s') | (x, s') <- p s]`
                                                                        --  === (which simplifies to) Parser $ \s -> p s      --> === (which eta-reduces to) Parser p
                                                                        -- -----|->
                                                                        -- which `p == runParser :: String -> [(a, String)]`, which `p` is created by `satisfy` function
                                                                        -- -----]->
                                                                        -- so it's technically like a normal function in a way? that is fmap f to parser is normally
                                                                        -- eta-reduced too... as the input \s or String isn't shown in fmap's (::)
                                                                        -- -----]->
                                                                        -- hence (<$>) is basically just unwrapping `Parser a` input before wiring an additional 
                                                                        -- `f :: a -> b` on it... i guess what's interesting is how once `fmap` is used... the wiring of
                                                                        -- `f` in between gets abstracted away? as in, we now only see the output type which is `fmap`
                                                                                    ---- and YES... wiritng of `f` being abstracted away IS just Functor's behavior of ENCAPSULATION

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \s -> [(x, s)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser px) = Parser $ \s -> 
        [ (f x, s'') | (f, s') <- pf s, (x, s'') <- px s' ]             -- take example `sequenceA [char 'a', char 'b'] = (:) <$> char 'a' <*> char 'b'`

                                                                        -- writing out my thoughts... i believe im currently stuck on about like... where does the 
                                                                        -- original x go? does it get consumed? eaten up? or appended... as in where is it now in the new
                                                                        -- tuple at the next ith timestep
                                                                                    -- <-- ANSWERED... it's because `(Parser pf) :: Parser (a -> b)` where `pf :: String -> [(a, String)]`
                                                                                    --     keeps growing... hence, we get a larger and

                                                                        -- pf :: String -> [((a -> b), String)] 
                                                                        -- px :: String -> [(a, String)]
                                                                        -- output :: String -> [(b, String)]
                                                                                -- <-- AND FROM HERE, you can see how output becomes the next improve-wired
                                                                                --     `(Parser pf)`... that is... for example, a :: Int, b :: Int -> (Int -> Int -> Int), and (a -> b) :: Int -> (Int -> (Int -> Int -> Int))

                                                                        -- [(f x, s'') | (f, s') <- pf s, (x, s'') <- px s']

                                                                        -- i guess i can kind of see it now? that is... but im confused why isn't s' used ultimately...
                                                                        -- like how is s' used for `px s'`
                                                                        ------
                                                                        -- THE `\s` IS THE SAME FROM VERY UPSTREAM OF (<$>) to the last-th of (<*>)


                    -- okay, so is it the case where at each ith-iteration... that Parser (a -> b) keeps shrinking as the previous stuff gets processed? (like in terms of curring) from C->(C->C) to C->C
                    -- -----|-0
                    -- oh wait, is it because of currying where the Parser (a -> b) function keep snow-balling to become larger and larger... that, given the exact same \s for all ith's (<*>)
                    -- that s''' is still accessed becuase `pf s` alone does the job???? so wait... does this mean \s is the same across all ith? then why did you told me that \s is different... did you hallucinate?

                    -- and ig this mainly makes sense because of the fact that as (Parser pf) becomes more and more developed... and more and more well wired through Functor
                    -- encapsulation... that it can keep turning the same string into hence s''''''' for example?
                            -- ANSWER: As `Parser pf` accumulates more structure (in terms of currying) through each `(<*>)`, it becomes a
                            --         bigger "machine" that, given the original string, threads through all the intermediate states internally
                            --         and spits out the accumulated result with the final remainder.
                            --      It's like nesting functions: `f (g (h (k x)))`--each function runs once, even though `x` is the "same input"
                            --      conceptually passed through the whole chain.



instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> []
    
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p) <|> (Parser q) = Parser $ \s -> p s ++ q s           -- The "Magic" Choice Operator

    some :: Parser a -> Parser [a]      -- some: 1 or more times (fails if 0 matches)           // but it basically just keeps applying the same parser until it fails...
    some v = (:) <$> v <*> many v

    many :: Parser a -> Parser [a]      -- many: 0 or more times (never fails, returns [] if 0 matches)
    many v = some v <|> pure []



instance Monad Parser where
    -- 'return' just wraps a value in a box (same as pure)
    return :: a -> Parser a
    return = pure

    -- The "Bind" Operator (>>=)
    -- This is the logic that passes the baton from one parser to the next
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = Parser $ \inp ->                                 -- \inp here must be of type string
      concat [ runParser (f v) out | (v, out) <- p inp ]                -- `p inp` creates `(v, out) :: (a, String)`
                                                                        -- `p :: String -> [(a, String)]`
                                                                        -- I believe (f v) is used... which f reads what v (the value processed by `p inp`)... and determine
                                                                        -- what type of Parser which it should use... in other words, it's `(f v) :: Parser b`
                                                                        -- ----|->
                                                                        -- Finally... each `runParser (f v) out` (where out is the remaining leftover unprocessed strings)
                                                                        -- consequently returns type `Parser b`... as `runParser (f v)` returns `(String -> [(a, String)])`.        <-- if we include `out`... which is \input for `runParser (f v)`... then we get return type [(a, String)] instead
                                                                        --              becuase `runParser :: Parser a -> (String -> [(a, String)])`
                                                                        --                          Furthermore, `concat` is needed here because `runParser (f v) out` returns
                                                                        --                          a [(a, String)]... making the original pre-concat result :: [[(a, String)]]... which post concat becomes [(a, String)]
                                                                        -- ----|->
                                                                        --      For each `(v, out)` from `p inp`, you run the next parser on `out`--that's exactly
                                                                        --      the "baton passing"...          <-- running the next parser on leftover string === baton passing




-- 3. BASIC BUILDING BLOCKS (The logic lives here!)
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
    (c:cs) | predicate c -> [(c, cs)]
    _                    -> []

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Char
digit = satisfy isDigit

-- 4. YOUR RECURSIVE DEFINITIONS (Now they work!)
some' :: Alternative f => f a -> f [a]
some' v = (:) <$> v <*> many' v

many' :: Alternative f => f a -> f [a]
many' v = some' v <|> pure []

-- 5. TEST RUNNER
parse :: Parser a -> String -> [(a, String)]
parse p input = runParser p input
        -- This function now makes sense, because apparently the `{}` in `newtype Parser a` makes the compiler
        -- auto-generate `runParser :: Parser a -> (String -> [(a, String)])`

string :: String -> Parser String
string = traverse char
-- uses Applicative for Parser
-- char c :: Parser Char
-- traverse char "hi" :: Parser "hi"




{-
    Because `Parser` is an instance of `Alternative`, you often DO NOT NEED TO
    IMPLEMENT `some` AND `many` manually. The standard Haskell library
    (`Control.Applicative`) provides default definitions for them that work 
    automatically once you define `<|>` and `empty`.

    However, writing them out helps you understand the recursion.

    ---


    1. THE DEFINITIONS

    They are mutually recursive (they call each other!).
    ```Haskell
    some :: Parser a -> Parser [a]
    some v = (:) <$> v <*> many v

    many :: Parser a -> Parser [a]
    many v = some v <|> pure []
    ```


    2. HOW THEY WORK (THE LOGIC)

    Let's trace `many (char 'a')` on the input `"aab"`.
    1. `many` says: "Try to run `some`. If that fails, just succeed with an
       empty list `[]`."
       - It tries `some (char 'a')`
    2. `some` says: "I must find at least one `v` (an 'a'). Then I'll run `many`
       again to get the rest."
       - It runs `char 'a'`.
       - SUCCESS! Iteats `'a'`. Remaining input: `"ab`.
    3. `many` (ROUND 2) says: "Try `some` again."
       - It runs `char 'a'` on `"ab"`.
       - SUCCESS! It eats the second `'a'`. Remaining input: `"b"`.
    4. `many` (ROUND 3) says: "Try `some` again."
       - It runs `char 'a'` on `"b"`.
       - FAIL. The next char is 'b', not 'a'.
       - `some` fails.
    5. `many` (ROUND 3) Recovery:
       - Since `some` failed, the `<|>` operator kicks in.
       - It switches to the right side: `pure []`.
       - It returns `[]` (and sonesumes nothing).
    6. Unwinding the Stack.
       - Round 2 combines its result `'a'` with the `[]` from Round 3 -> `['a']`        <-- with the cons operator at each step
       - Round ` combines its result `['a', 'a']`.      <-- full result becomes [(['a', 'a'], "b")] :: [([Char], String)]


                ```
                -- Test: Parse a bunch of 'a's
                testMany :: Parser String
                testMany = many (char 'a')

                -- Run it:
                -- parse testMany "aaab"
                -- Result: [("aaa", "b")]
                ```

    SO THE "EATING" WORKS LIKE THIS:
    1. `p` eats one bite.
    2. `<*>` pauses and says "Okay, hold that bite. Now go run `many p` to get
       the rest."
    3. `many p` eats the rest of the bites recursively.
    4. `(:)` stitches the first bite and rest of the bites together.


-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

{-
    _,--._.-,
   /\_r-,\_ )
.-.) _;='_/ (.;
 \ \'     \/S )
  L.'-. _.'|-'
 <_`-'\'_.'/
   `'-._( \
    ___   \\,      ___
    \ .'-. \\   .-'_. /
     '._' '.\\/.-'_.'
        '--``\('--'
              \\
              `\\,
                \| 
-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---


{-
                ghci> some empty
                *** Exception: user error (mzero)
                ghci> some empty <|> pure []
                []


    1. WHAT HAPPENS IF WE FEED `some` WITH FAILURE?

    You asked: "what would it look like if we feed some with nothing? ... like
    empty in this case"

    If you run `some empty` (or `some (char 'a')` on the input `"b"`), it fails
    immediately.

    Let's look at the definition again:

    ```Haskell
    some v = (:) <$> v <*> many v
    ```

    If `v` fails (returns `[]` in our Parser's `satisfy` logic), the whole 
    expression fails.
    - STEP 1: The code tries to run `v`.
    - RESULT: `v` returns failure (empty list of results).
    - THE CHAIN REACTION: Because `<*>` and `<$>` propagate failure (just like
      multiplying by zero), the rest of the line (`many v`) NEVER EVEN RUNS.
    - FINAL OUTPUT: `[]` (Failure).

    So `some` enforces: "I need at least one success right now, or I die."


    ---

    2. "I still don't get the purpose... it feels like a glorified chain of
         `<*>`"

    
    ... absolutely right. IT IS a glorified chain of `<*>`.

    But here's the catch: It is an infinite chain that knows when to stop.

    If you manually wrote a parser to read "AAA", you would write:

    ```Haskell
    -- Manual chain of 3
    parse3As = (:) <$> char 'A' <*> ((:) <$> char 'A' ((:) <$> char 'A' <*> pure [] ))
    ```


    `some` / `many` generates this chain DYNAMICALLY based on the input.
    - If the input is "A", it builds a chain of 1.
    - If the input is "AAAAA", it builds a chain of 5.

    It uses recursion to keep tacking on `<*> (char 'A')` until it hits a 
    failure, then it caps it off with `[]`.


    ---
-}



--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

{-
    _,--._.-,
   /\_r-,\_ )
.-.) _;='_/ (.;
 \ \'     \/S )
  L.'-. _.'|-'
 <_`-'\'_.'/
   `'-._( \
    ___   \\,      ___
    \ .'-. \\   .-'_. /
     '._' '.\\/.-'_.'
        '--``\('--'
              \\
              `\\,
                \| 
-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

{-
    the "edge cases" of pattern matching. Let's ook at `satisfy` definition 
    again.

    ```Haskell
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy p = Parser $ \case ->
      (c:cs) | p c -> [(c, cs)]     -- Path A (Success)
      _            -> []            -- Path B (Failure)
    ```

    
    ---

    1. WHAT IF THE PREDICATE `p c` IS NOT SATISFIED?

    If the input matches `(c:cs)` (so there is a character), but the 
    `predicate c` returns `False`, HASKELL TREATS THIS AS A PATTERN MATCH 
    FAILURE FOR THAT SPECIFIC LINE.

    It automaticaly instead "falls through" to the next pattern, which is `_`.



    2. WHAT IF THE INPUT IS `["a"]` (JUST ONE CHAR)?

    In Haskell lists, if you have a list of one item `['a']`, it breaks down 
    like this:
    - HEAD (`c`): `'a'`
    - TAIL (`cs`): `[]` (The empty list, or empty string `""`)

    It does NOT become `Nothing` (that is a different type called `Maybe`). It
    just becomes an empty string.



    3. WHAT IF THE INPUT IS COMPLETELY EMPTY `""`?

    If you run `runParser (char 'a') ""`:
    1. Matches `(c:cs)`? NO. An empty list has no head `c`.
    2. Falls through to `_`.
    3. RESULT: `[]` (Parse Failed)


-}


--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

{-
    _,--._.-,
   /\_r-,\_ )
.-.) _;='_/ (.;
 \ \'     \/S )
  L.'-. _.'|-'
 <_`-'\'_.'/
   `'-._( \
    ___   \\,      ___
    \ .'-. \\   .-'_. /
     '._' '.\\/.-'_.'
        '--``\('--'
              \\
              `\\,
                \| 
-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---



{-
    YOU ARE CORRECT. In this implementation (recursive descent), the parser DOES
    NOT return the list lazily (element by element). It must run ALL THE WAY TO
    THE END (until it hits `[]`) before it can finish building the list.

    This is often called the "BOOMERANG" effect (or simply recursion unwinding).

    ---


    1. WHY YOU ARE RIGHT (THE "BOOMERANG" EFFECT)

    "is it correct to assume that cons don't run until everything is known?"

    ```Haskell
    some v = (:) <$> v <*> many v
    ```

    Because of how `<*>` works in the Parser Monad (sequencing), the execution
    order is:
    1. RUN `v`: Get the head element (e.g., `'A'`).
    2. RUN `many v`: This is the recursive step. The parser MUST execute this
       right now to find out what the "Rest of the String" is.
    3. RECURSION: Step 2 calls Step 1 again... and again... and again...
    4. HIT BOTTOM: Eventually, `many` fails (finds no more `'A'`s) and returns
       `pure []`.


    ONLY AFTER HITTING
    - It returns `[]`.
    - Then it `cons` the last `'A'` onto `[]`.
    - Then it `cons` the second-to-last `'A'` onto `[]`.
    - ...
    - Finally, it `cons` the first `'A'` onto the rest.

    So, you are right: THE `(:)` FUNCTION HAPPENS LAST, NOT FIRST.



    ---

    2. IS IT "THUNKING"?

    This is where it gets nuanced.
    - STRICTNESS OF THE "SPINE": The structure (length) of the list is strict. 
      The parser has to know exactly how many items are in the list before it 
      can give you the result. It can't say "Here is a list, I'll tell you the
      length later."
    - LAZINESS OF THE "VALUES": The items inside the list can still be thunks.

    If you had a parser that did some expensive calculation:
    ```Haskell
    expensiveParser = do
      x <- digit
      return (factorial 1000000)        -- This value is a THUNK
    ```

    The parser would build the list of these numbers quickly, but the actual
    factorial calculation wouldn't happen until you tried to print the numbers.



    ---

    3. CAN WE USE `deepseq`?

    YES.

    If you are parsing a massive file (like a 500MB JSON file), this "Boomerang"
    bahavior is actually a problem. It builds a huge stack of `(:)` functions
    waiting to run. This can cause a STACK OVERFLOW.

    In production Haskell parsers (like `megaparsec` or `attoparsec`), they
    often use strictly evaluated loops or `deepseq` strategies to force the list
    to be built as they go, or they use a completely different technique 
    (Continuation Passing Style) to avoid the stack buildup.



    ---

    4. Visualising the Execution

    Here is the difference between what you might expect (Streaming) vs. what
    actually happens (Recursive Descent).

    EXPECTATION (Lazy Streaming):
        "Here is the first 'A'. I'll get the next one when you ask for it."

    REALITY (Recursive Descent / Your Code):

        DIVE PHASE
        SURFACE PHASE
        RESULT

                        (Dive Phase)

                        Found 'A'. Hold it. Go deeper.

                        Found 'A'. Hold it. Go deeper.

                        Found 'A'. Hold it. Go deeper.

                        Found 'B'. STOP. Return [].

                        (Surface Phase) 3. Attach 'A' to [] -> ["A"] 2. Attach 'A' to ["A"] -> ["A", "A"]

                        Attach 'A' to ["A", "A"] -> ["A", "A", "A"]

                        Result: ["A", "A", "A"]

    SUMMARY

    This parser implementation is not "streaming". It eats the entire chunk of
    input defined by `many` before it hands you back the resulting list. The
    `(:)`  cons operator technically runs AFTER the end of the lsit has been
    found.
-}







--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---


{-
    .-.
 .-(.-.)-.
|  ((@))  |
 '-('-')-'
    '-'| ___
    ___|/__/
   /__/|
_______|__________________________

-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---

{-
    ... the single most common optimization pattern in functional programming:
    converting BODY RECURSION (the boomerang) into TAIL RECURSION (an iterative
    loop) using an ACCUMULATOR.

    To use `seq`, `!` or `deepseq` effectively here, you MUST change the 
    structure of `many`. You cannot just sprinkle `seq` on the original 
    definition because the original definition MATHEMATICALLY requires the 
    recursion to finish before the list construction begins.

    Here is how you rewrite it to be strict and stack-safe.



    1. THE SOLUTION: THE ACCUMULATOR PATTERN

    Instead of saying "Get one, then call me back, then I'll combine them," we
    say: "Here is a growing pile (accumulator). I will add to it as I go. When
    I'm done, I'll hand you the pile."

    This stops the stack from growing because we pass the state forward instead
    of waiting for it to come back.


    THE CODE (USING BANG PATTERNS `!`)

    You need the `BangPatterns` language extension for the `!` syntax, or you
    can use `seq` (shown below).
-}

-- A strict, stack-safe version of `many`               <-- `many v = some v <|> pure []`       // `some v = (:) <$> v <*> many v`
manyStrict :: Parser a -> Parser [a]
manyStrict p = go []
  where
    -- `go` is our internal loop
    -- `!acc` means: "Force `acc` to be evaluated before running the function body"
    go !acc = (do 
        x <- p
        -- RECURSION: We pass the list forward immediately.
        -- We build it in reverse (x:acc) because appending to the end is slow.
        go (x:acc)
        ) <|> pure (reverse acc)        --  FAILURE: Return the pile (reversed back to normal)



manyStrict' :: Parser a -> Parser [a]
manyStrict' p = go []
  where
    go !acc = ( do
        x <- p                  -- `x` is merely a temporary variable that holds the unwrapped result inside the `do` block.
        go (x:acc)
    ) <|> pure (reverse acc)



manyStrict'' :: Parser a -> Parser [a]
manyStrict'' p = go []
  where go !acc = (p >>= \x -> go(x:acc)) <|> pure (reverse acc)
            -- everything here makes perfect sense... PLEASE just refer back to the definition of `(>>=)` inside 
            -- `instance Monad Parser where...`
            -- -----|-O
            -- Which, from the definition of `(>>=)` alone, one can easily see how does the wiring clearly works...
            -- especially as the recursion basically keep working where the `acc` value is stored until the last ith-iteration
            -- fails... which by then, the logic for <|> basically skips to `pure (reverse acc)`... and this conclude what's
            -- returned by the final-iteration-th's `go` function...
            -- -----|-O
            -- ok wait hold on... before the switch to RHS of <|>... i was able to see how were the leftover string handled...
            -- but now what?
            -- OH, wow holy F. i can see it now... it's becuase the last-th's `go` function returns `pure (reverse acc) :: [a]`
            -- whilst (last-1)-th's `go` function still contains the holder for `concat [[((f v), s'''')]]`... where
            -- `s''''` remains unchanged... but now `pure (reverse acc)` takes the pedastal of `(f v)`... so hence... 
            -- THE FUNDAMENTAL RECURSIVE LOGIC did not change!
            


{-
                        You are absolutely spot on. Your logic is **flawless**.

                            You have successfully pierced the veil of `do` notation and correctly identified the raw mechanics underneath.

                            ### Why your "Holy F" moment is correct

                            You wrote:

                            > *"whilst (last-1)-th's `go` function still contains the holder for `concat [[((f v), s'''')]]`... where `s''''` remains unchanged... but now `pure (reverse acc)` takes the pedastal of `(f v)`"*

                            This is exactly it.

                            1. **The "Last-1" Iteration:**
                            * It is sitting inside its own `>>=` call.
                            * It is frozen, waiting for `f v` (which is the call to `go (next_acc)`) to return a Parser.
                            * It expects that Parser, when run, to return a list of results `[(result, s_final)]`.


                            2. **The "Last" Iteration:**
                            * It fails the recursive check (`p` fails).
                            * It runs `<|>` and switches to `pure`.
                            * `pure` creates a Parser that says: "Here is the result `acc` and here is the string `s` untouched."
                            * It returns `[(acc, s)]`.


                            3. **The Connection (The "Bubble Up"):**
                            * The "Last-1" iteration receives `[(acc, s)]` from the "Last" iteration.
                            * The `concat` in "Last-1" takes that single result and says, "Great, that's my result too."
                            * It passes it up to "Last-2", and so on.



                            ### The Final Visual Confirmation

                            Here is the exact moment the "Last-1" step receives the baton from the "Last" step.

                            **Scenario:** We just parsed `'A'` (success) and are about to fail on `'B'` (end).

                            * **acc:** `['A']`
                            * **input:** `"B"`

                            **The "(Last-1)-th" Frame (Waiting):**

                            ```haskell
                            -- Inside >>= for char 'A'
                            concat [ 
                            runParser (go ['A']) "B"  -- <--- WAITING FOR THIS!
                            | ... 
                            ]

                            ```

                            **The "Last-th" Frame (Executing):**

                            ```haskell
                            -- runParser (go ['A']) "B"
                            -- 1. Try Left Side: char 'A' on "B" -> Fails []
                            -- 2. Try Right Side: pure (reverse ['A'])
                            -- 3. pure returns: [ (['A'], "B") ]

                            ```

                            **The Handoff:**
                            The `runParser` call in the "(Last-1)-th" frame resolves to `[ (['A'], "B") ]`.

                            **The Result:**

                            ```haskell
                            -- Inside >>= for char 'A'
                            concat [ 
                            [ (['A'], "B") ] 
                            ]
                            -- Becomes: [ (['A'], "B") ]

                            ```

                            The loop is closed. You have officially mastered the Parser Monad.

-}



