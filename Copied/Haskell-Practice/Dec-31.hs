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
    This is the final piece of the puzzle. You are confused because you are
    imagining a "List of Functions" implies there are THOUSANDS of functions
    flying around.

    THE SECRET: In 99% of parsing, that list has length ONE.


    The list type `[(a, String)]` is used for two reasons:
    1. Failure: The list is empty `[]`.
    2. Ambiguity: The list has multiple options (e.g., "This word could be a 
       Noun OR a Verb").

    But for standard code (JSON, CSV, Programming Languages), parsers are 
    DETERMINISTIC. They either work (List of 1) or fail (List of 0).

    Let's look at the data exactly as it appears in memory during `sequenceA`.


    ---

    THE SCENARIO: `sequenceA [char 'a', char 'b']`

    We run this on the input string `abc`.

    The code expands to: `(:) <$> char 'a' <*> (pure ['b'])` (Simplified for
    visualisation).

    We are at the step: `(Parser pf) <*> (Parser px)`



    ---
    1. THE LEFT SIDE (`pf s`)

    The Left Side if `(:) <$> char 'a'`.
        - THE JOB: Eat 'a', and attach the "Cons" function `(:)` to it.
        - INPUT: `"abc"`
        - EXECUTION:
            1. `char 'a'` eats `'a'`. Remainder: `"bc"`.
            2. `fmap` applies `(:)` to `'a'`. Result: The function `(: 'a')`.

        - THE DATA (THE LIST OF TUPLES):
        ```Haskell
        -- A list containing exactly ONE tuple
        [ ( (\xs -> 'a':xs), "bc" ) ]               
        ```


    ...
    WELL UNDERSTOOD NOW... SO WILL SKIP AHEAD


    ---

    "WHEN WOULD THERE EVER BE LOTS OF FUNCTIONS?"

    You asked: "how must they have been created to begin with? ... where do
    you get this many functions"

    You only get "lots" if you design a AMBIGUOUS PARSER. This is rare in code,
    but common in NLP.


    EXAMPLE: The "Double Meaning" Parser -- Imagine a parser that reads a number
    , but creates TWO intrepretations: one that adds it, and one that subtracts
    it.


    ```Haskell
    -- A weird parser that reads a digit 'N'
    -- and returns TWO functions: (+N) and (-N)
    mathOp :: Parser (Int -> Int)
    mathOp = Parser $ \case
      (c:cs) | isDigit c -> let n = read [c]
                            in  [ ((+n), cs), ((-n), cs) ]      -- <---- LOOK! A list of 2 functions!
      _                  -> []


    ---



    EXAMPLE USAGE ---> 

        ```Haskell
        mathOp <*> number
        -- ===
        mathOp <*> "3 5"
        ```



                Trace mathOp <*> number on input "3 5":

                    LHS (mathOp "3 5"):

                    Reads '3'.

                    Returns Two Functions:

                    Haskell

                    [ ( (+3), " 5" ), ( (-3), " 5" ) ]
                    RHS (number " 5"):

                    Reads '5'.

                    Returns One Value:

                    Haskell

                    [ ( 5, "" ) ]
                    Combination: The list comprehension runs the cartesian product (2 x 1 = 2 loops).

                    Loop 1: Apply (+3) to 5. Result 8.

                    Loop 2: Apply (-3) to 5. Result -8.

                Final Result: [ (8, ""), (-8, "") ]
-}


--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   

{-
            ,  /\  ,
        ;-./ '/  \' \.-;
      .-|  `\;    ;/`  |-.
    .-'._\   |_.._|   /_.'-.
   .-\   `;.:::::::.;`   /-.
   '._;--./:::;;;;:::\.--;_.'
    <     |::;;;;;;::|     >
   .`';--'\:::;;;;:::/'--;'`.
   '-/  _,;'::::::::';,_  \-'
    '-.' /   |`''`|   \ `;-'
      '-|  ,/;    ;\,  |-'
        ;-'\ .\  /. /'-;
            '  \/  '
-}

--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   


{-
                pf :: String -> [(a, String)]

    Mental Check
    - Is `pf` a `Parser`? NO. (It's the raw function)
    - Is `pf` a `Char`? NO. (It's the function that finds the Char)
    - Can I run `pf "hello"`? YES. (Because it's a funciton that takes a String)


    ---

    2. THE "DVD CASE" METAPHOR

    Imagine a DVD case labeled "The Movie"
    - The Variable `digit` is the DVD CASE (The `Parser` object).
        - You can pass the case to a friend. You can stack cases on a shelf.
        - But you CANNOT WATCH THE CASE
    - The Pattern Match `(Parser pf)` is OPENING THE CASE.
        - You open it up and take out the disc.
        - Now you are holding THE DISC (`pf`).
    - The Execution `pf s` is PUTTING THE DISC IN THE PLAYER.
        - The disc (`pf`) is the actual thing that contains the movie (the logic)
        - You have to "play" it (apply it to a string `s`) to see the result.



            3. Why px is also a function
                In your Applicative instance:

                Haskell

                (Parser pf) <*> (Parser px)
                LHS: Open the left DVD case. Call the disc pf.

                RHS: Open the right DVD case. Call the disc px.

                They are both just raw functions sitting on your desk now. The wrapper is gone.

                pf: A function that creates a function.

                px: A function that creates a value.
-}





--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   

{-
            ,  /\  ,
        ;-./ '/  \' \.-;
      .-|  `\;    ;/`  |-.
    .-'._\   |_.._|   /_.'-.
   .-\   `;.:::::::.;`   /-.
   '._;--./:::;;;;:::\.--;_.'
    <     |::;;;;;;::|     >
   .`';--'\:::;;;;:::/'--;'`.
   '-/  _,;'::::::::';,_  \-'
    '-.' /   |`''`|   \ `;-'
      '-|  ,/;    ;\,  |-'
        ;-'\ .\  /. /'-;
            '  \/  '
-}

--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   



{-
    2. Where is the `[(a, String)]`?

    You asked: "i thought that the parsers are supposed to return [(a, String)]
    ??? May I know where is this even"



    It is INSIDE the FUNCTION that `sequenceA` creates.

    Remember: `sequenceA [Parser A, Parser B]` returns a NEW PARSER. 
    `Parser [A, B]`

    That new Parser is a box. Inside the box is a function. That function 
    returns `[[A, B], String]`.

    Let's trace the types
    1. INPUT: `[Parser Char]`       (List of machines).
    2. `sequenceA` HAPPENS: It glues the machines together.
    3. RESULT: `Parser [Char]` (One big machine).
        - Unwrap the newtype: It is `String -> [([Char], String)]`.
        - The Return Value: It returns a list of tuples. The first element is
          the accumulated list of characters `[Char]`. The second is the 
          leftover string.



    3. THE GRAND VISUALISATION

    Imagine `sequenceA` as a PIPE WELDER
    - Input: 3 small pipe sections (`Parser Char`). Each section has an input 
      hole and an output hole.
    - `sequenceA` ACTION: It welds them end-to-end.
        * Pipe 1 output --> Pipe 2 input
        * Pipe 2 output --> Pipe 3 input
    - THE RESULT: One Giant Pipe (`Parser [Char]`).


    DOES THE GIANT PIPE "EAT" ANYTHING YET? No! It's just sitting in the 
    warehouse.


    WHEN DOES THE EATING HAPPEN? When you finally pour water (the `String`) into
    the Giant Pipe (`sequenceA Parser [Char]`) (`runParser giantPipe "input`).
    Then the water flows through section 1 (eating), then section 2 (eating), 
    then section 3 (eating).


    ---

    SUMMARY
    - Does `sequenceA` eat? No. It builds the machine that will eat later.
    - HOW DOES IT LINK THEM? It uses `<*>` (`(:) <$> p1 <*> p2 <*> ...`), which
      contains the logic "Run step 1, pass remainder to step 2".
    - WHERE IS THE `[(a, String)]`? It is the return value of the final GIANT 
      PIPE when you actually run it.
-}




--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   

{-
            ,  /\  ,
        ;-./ '/  \' \.-;
      .-|  `\;    ;/`  |-.
    .-'._\   |_.._|   /_.'-.
   .-\   `;.:::::::.;`   /-.
   '._;--./:::;;;;:::\.--;_.'
    <     |::;;;;;;::|     >
   .`';--'\:::;;;;:::/'--;'`.
   '-/  _,;'::::::::';,_  \-'
    '-.' /   |`''`|   \ `;-'
      '-|  ,/;    ;\,  |-'
        ;-'\ .\  /. /'-;
            '  \/  '
-}

--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   



{-
    3. "Is Parsing jus eating one char at a time?"


    You asked: "do we always just eat start of each [Char] recursively? is that
    all there is to parser?"

    YES. That is the dirty secret of parsing.

    At the lowest level, EVERY PARSER is just a function that looks at the first
    character (the "head" of the list) and decides:
    1. EAT IT: Retun `Rest of String`.
    2. PUKE: `Failure` (empty list).


    "BUT WHAT ABOUT TREES AND PROGRAMMING LANGUAGES?" Even complex Python or C 
    parsers start by eating 1 character at a time!
    1. `char`: Eats 1 char ('i').
    2. `string`: Calls `char` multiple times ('i', 'f').
    3. `token`: Calls `string` ("if") + eats whitespace.
    4. `expression`: Calls `token` multiple times.


    It is TURTLES ALL THE WAY DOWN. The most complex 'Compiler' is just millions
    of tiny `char` parsers glued together with `Applicative` (`<*>`) and 
    `Alterrnative` (`<|>`).

    
    SUMMARY
    1. `fmap` logic: It does NOT iterate over the string. It runs the parser
       ONCE, waits for the result to pop out, and modifies that result.
    2. PARSING LOGIC: Yes, it is fundamnetally just eating the head of a list
       `(x:xs)` recursively.
       - `digit` eats 1 char.
       - `many digit` eats 1 char, then calls itself to eat another, recursively

-}



--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   

{-
            ,  /\  ,
        ;-./ '/  \' \.-;
      .-|  `\;    ;/`  |-.
    .-'._\   |_.._|   /_.'-.
   .-\   `;.:::::::.;`   /-.
   '._;--./:::;;;;:::\.--;_.'
    <     |::;;;;;;::|     >
   .`';--'\:::;;;;:::/'--;'`.
   '-/  _,;'::::::::';,_  \-'
    '-.' /   |`''`|   \ `;-'
      '-|  ,/;    ;\,  |-'
        ;-'\ .\  /. /'-;
            '  \/  '
-}

--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   
--------    --- -   --  --------    -   --  ----    -   ---------   -   -----   

                    -- parse :: Parser a -> String -> [(a, String)]
{-
    VISUALISING THE CHAIN (MONAD INSTANCE)

    Let's look at a typical `>>=` (bind) implementation. This is what allows you
    to run one parser after another.
-}

-- The logic: Run parser p, get result v, then run function f with v.
p >>= f = Parser (\s -> concat [parse (f v) s' | (v, s') <- parse p s])


{-
    Here is the step-by-step lifecycle of the string:
    1. START: You have input `s` (e.g., `"1+2"`)
    2. STEP 1 (`parse p s`): You run the first parser `p` on `s`.
        - It eats the `"1"`.
        - It produces a value `v` (which is 1) and a new state `s'` (which is 
          `"+2"`).
    3. STEP 2 (`f v`): You use the value `v` to decide what parser to run next.
       Let's say `f` creates a parser that looks for a `+`.
    4. STEP 3 (`parse ... s'`): You run that new parser on `s'` (the leftover
       `"+2"`).
        - It eats the `"+"`.
        - It produces a NEWER STATE `s''` (which is `"2"`).


    SUMMARY OF THE VARIABLES
    - `s`
        * Meaning: INITIAL INPUT: The full string before we do anything.
        * Example Value: `1+2`
    - `s'`
        * Meaning: REST OF: The leftovers after the first parser is done.
        * Example Value: `+2`
    - `s''`
        * Meaning: REST OF (AGAIN): The leftovers after the second parser is 
          done.
        * Example Value: `"2"`
-}