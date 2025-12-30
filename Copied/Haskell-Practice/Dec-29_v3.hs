import Data.Char

{-
    THE CORE IDEA: WHAT IS PARSING?

    Parsing is taking unstructured text (a `String`) and extracting structured
    data from it. But parsing can:
    
    - Fail (the input doesn't match what we expect)
    - Succeed in Multiple Ways (ambiguous grammar)
    - Leave Leftover Input (we only consumed part of the string)

    So a parser for type `a` is a function that takes a string and returns a 
    list of possible results, each paired with the unconsumed remainder:

    ```Haskell
    newtype Parser a = Parser (String -> [(a, String)])
    ```         <-- the function above are treated as only one-value...

    This list handles all three concerns:
    - Empty List `[]`  = failure
    - Multiple elements = ambiguity
    - The `String` in each pair = leftover input


    ---

    THE PRIMITIVE: `satisfy`

    Everything builds from one primitive parser:
    ```Haskell
    satisfy :: (Char -> Bool) -> Parser Char
    satisfy f = Parser eat
      where eat (c:cs) | f c = [(c, cs)]    -- consume if f c is true
            eat _            = []           -- otherwise fail
    ```

    It tries to eat one character from the input, but only if it passes the test
    `f`.

    EXAMPLES:
    ```Haskell
    digit :: Parser Char
    digit = satisfy isDigit

    parse digit "42"   = [('4', "2")]   -- ate '4', left "2"
    parse digit "abc"  = []              -- failed, 'a' isn't a digit
    parse digit ""     = []              -- failed, nothing to eat
    parse digit "3abcasas"  = [('3', "abcasas")]
    ```


    ---


        3. How do you USE it? (Opening the Box)

        Since `digit` is a Box, you can't just feed it a string. You have to
        unwrap it first.

        Usually libraries provide a function called `runParser` (or you pattern
        match):

        ```Haskell
        -- We define an "Unrapper function"
        runParser :: Parser a -> (String -> [(a, String)])
        runParser (Parser f) = f    -- Pattern match to get the function `f` out
        ```

        ...

        Summary of the "Type Switch"

                You asked: "is the parser newtype stuff seen when declaring functions... but when it's being used insdie the function itself... it behaves more like the latter???"

        YES!
        - PUBLIC FACE (TYPE SIGNATURE): `Parser Char`. Everyone else sees the 
          Box. They can pass the Box around, put the Box in a list, etc.
        - PRIVATE LIFE (IMPLEMENATION): `String -> [(Char, String)]`. When you
          are writing the parser logic (like inside `eat`), you are dealing with
          the raw strings and lists. You only put the Box on at the very last
          second before returning.
-}


{-
    WHY THIS IS POWERFUL

    This is the standard pattern for defining things like State Monads, Reader
    Monads, and Parsers.

    Because functions are First-Class Citizens (Values), wrapping a function in
    a `newtype` is exactly the same as wrapping an `Int`.
    1. Wrapper: `newtype Age = Age Int`
       - Inside: A chunk of data (an Integer)
    2. Wrapper: `newtype Parser a = Parser (String -> ...)`
       - Inside: A chunk of logic (a Function)

    To the `newtype` keyword, both `Int` and `(String -> ...)` are just "One 
    Thing" to be wrapped.
-}


------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----
------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----

{-
         ,            __ \/ __
     /\^/`\          /o \{}/ o\   If I had a flower for each time
    | \/   |         \   ()   /     I thought of you, my garden
    | |    |          `> /\ <`   ,,,     would be full...
    \ \    /  @@@@    (o/\/\o)  {{{}}                 _ _
     '\\//'  @@()@@  _ )    (    ~Y~       @@@@     _{ ' }_
       ||     @@@@ _(_)_   wWWWw .oOOo.   @@()@@   { `.!.` }
       ||     ,/  (_)@(_)  (___) OO()OO    @@@@  _ ',_/Y\_,'
       ||  ,\ | /)  (_)\     Y   'OOOO',,,(\|/ _(_)_ {_,_}
   |\  ||  |\\|// vVVVv`|/@@@@    _ \/{{}}}\| (_)@(_)  |  ,,,
   | | ||  | |;,,,(___) |@@()@@ _(_)_| ~Y~ wWWWw(_)\ (\| {{{}}
   | | || / / {{}}} Y  \| @@@@ (_)#(_) \|  (___)   |  \| /~Y~
    \ \||/ /\\|~Y~ \|/  | \ \/  /(_) |/ |/   Y    \|/  |//\|/
   \ `\\//`,.\|/|//.|/\\|/\\|,\|/ //\|/\|.\\\| // \|\\ |/,\|/
-}

{-
    MAKING PARSER A FUNCTOR

    If we can parse a `Char`, we might want to transform it into something else
    (like an `Int`). That's what Functor gives us:

    ```Haskell
    instane Functor Parser where
      fmap f (Parser p) = Parser $ \cs ->
        [(f x, rest) | (x, rest) <- p cs]       <-- p is a function that takes in cs and outputs somethng else...
    ```
    
    We run the parser, then apply `f` to every successful result.

    ```Haskell
    parse (ord <$> digit) "42" = [(52, "2")]        -- ord '4' = 52
    ```

-}
newtype Parser a = Parser (String -> [(a, String)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser eat
  where eat (c:cs) | f c = [(c, cs)]        -- checks, "is the first char a digit?"
        eat _            = []

digit :: Parser Char
digit = satisfy isDigit

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) cs = p cs

        {-
            Nothing magical: `parse` just unwraps `Parser` and calls the 
            underlying function. 

            Why a list of results? So the parser can represent:
            - failure: `[]`
            - success: `[(result, rest)]`
            - ambiguity/backtracking: `[(r1, rest1), (r2, rest2), ...]`
        -}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \cs ->
    [(f x, rest) | (x, rest) <- p cs]
    -- for `digit :: Parser Char` for example, it relies on 
    -- `satisfy :: (Char -> Bool) -> Parser Char`
    --
    -- Which means `p cs` returns a list of parsed chars... such as [('7', "abc...")]


-- We run the parser, then apply `f` to every successful result

pf = parse $ 

------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----
------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----


{-


-}
------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----
------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----





{-
         wWWWw               wWWWw
   vVVVv (___) wWWWw         (___)  vVVVv
   (___)  ~Y~  (___)  vVVVv   ~Y~   (___)
    ~Y~   \|    ~Y~   (___)    |/    ~Y~
    \|   \ |/   \| /  \~Y~/   \|    \ |/
   \\|// \\|// \\|/// \\|//  \\|// \\\|///
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

{-
    

    2) "DETERMINISTIC" vs "NON-DETERMINISTIC"

    A parser is DETERMINISTIC (in this list-of-results sense) if for every input
    `s` it returns AT MOST ONE result:

    ```Haskell
    length (parse p s) <= 1
    ```

    Your `digit` is deterministic because `satisfy` only ever returns `[]` or a
    singleton list:

    ```Haskell
    satisfy f = Parser $ \case
      (c:cs) | f c -> [(c, cs)]
      _            -> []
    ```

    There is no way for it to produce two different outputs on the same input.

    A parser is NON-DETERMINISTIC if on some input it can return two or more
    results.


    ---

    3) WHERE DO AMBIGUITY/BACKTRACKING COME FROM?

    They come from COMBINATORS that combine parsers, especially choice.

    A typical `Alternative` choice for this representation is:

    ```Haskell
    empty :: Parser a
    empty = Parser $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) (Parser p) (Parser q) = Parser $ \s -> p s ++ q s
    ```

    T



            ---

            In computer science, deterministic means a system or algorithm 
            always produces the exact same output for the same input, with 
            predictable, non-random steps, making debugging easier ensuring
            reliability, unlike probabilistic systems that involve chance.

            ---

            Nondeterminism means that the path of execution isn't fully 
            determined by the specification of the computation, so the input
            can produce different outcomes, whilst deterministic execution is
            guaranteed to be the same, given the same input.




            ---

            Special Syntactic Sugar --> `LambdaCase`

            STANDARD SYNTAX (without `\case`): You have to explicitly name the
            argument (let's call it `input`), and then switch on it.
            ```Haskell
            -- The Standard Way
            satisfy f = Parser $ \input -> case input of
              (c:cs) | f c -> [(c, cs)]
              _            -> []
            ```

            WITH `\case`: You delete the variable name (`input`) and the 
            `-> case ... of` part.
            ```Haskell
            -- The LambdaCase way
            satisfy f = Parser $ \case
              (c:cs) | f c -> [(c, cs)]
              _            -> []
            ```
-}


-----   -   --- -   ------  -   -   --  ------  -   ------  -   ----    -   ----
{-
        ,,,                      ,,,
       {{{}}    ,,,             {{{}}    ,,,
    ,,, ~Y~    {{{}},,,      ,,, ~Y~    {{{}},,,
   {{}}} |/,,,  ~Y~{{}}}    {{}}} |/,,,  ~Y~{{}}}
    ~Y~ \|{{}}}/\|/ ~Y~  ,,, ~Y~ \|{{}}}/\|/ ~Y~  ,,,
    \|/ \|/~Y~  \|,,,|/ {{}}}\|/ \|/~Y~  \|,,,|/ {{}}}
    \|/ \|/\|/  \{{{}}/  ~Y~ \|/ \|/\|/  \{{{}}/  ~Y~
    \|/\\|/\|/ \\|~Y~//  \|/ \|/\\|/\|/ \\|~Y~//  \|/
    \|//\|/\|/,\\|/|/|// \|/ \|//\|/\|/,\\|/|/|// \|/
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}

{-
    1. THE `Maybe` Instance (THE "BACKUP" PLAN)

    This is the cleanest mental model. It returns the first `Just` value it 
    encounters (left-biased).
    - If the left is `Just`, it wins (Right side is ignored).
    - If the Left if `Nothing, it tries the Right.

    ```Haskell
    import Control.Applicative

    -- Left succeeds, so it returns Left
    Just 5 <|> Just 10      -- Result: Just 5

    -- Left fails, so it returns Right
    Nothing <|> Just 10     -- Result: Just 10

    -- Both fail
    Nothing <|> Nothing     -- Result: Nothing

    -- Chaining (pick the first non-Nothing value)
    Nothing <|> Nothing <|> Just 99 <|> Just 100    -- Result: Just 99
    ```


    2. THE LIST `[]` INSTANCE (NON-DETERMINISM)

    For lists, "Alternative" means all possible results." Therefore, `<|>` acts
    exactly like list concatenation (`++`).

    ```Haskell
    [1, 2] <|> [3, 4]       -- Result: [1, 2, 3, 4]
    []     <|> [5]          -- Result: [5]
    ```

    Note: In the context of logic/nondeterminism monads, this represents "The
    answer could be 1 OR 2 OR 3 OR 4".



    3. THE PARSER INSTANCE (THE EXAM FAVOURITE)

    If your exam covers PARSERS (like `Parsec` or a custom `newtype Parser`), 
    this is where `<|>` is most important.

    It represents CHOICE.

    SCENARIO: You want to parse a variable name that starts with a letter, OR
    an underscore.
    ```Haskell
    -- Pseudocode for a Parser definition
    parseVariableStart :: Parser Char
    parseVariableStart = letter <|> char '_'
    ```

    HOW IT WORKS DURING EXECUTION:
    1. The parser looks at the input stream.
    2. It tries to run `letter`.
        - SUCCESS: It consumes the letter and returns it. `char '_'` is never
          touched.
        - FAILURE: (e.g., the input is `_var`), `letter` fails. The parser
          BACKTRACKS (resets the input) and tries `char '_'`.


    REAL-WORLD EXAM EXAMPLE
    ```Haskell
    -- A parser that accepts "True" or "False" to return a Boolean
    parseBool :: Parser Bool
    parseBool = (String "True" *> pure True) <|> (string "False" *> pure False)
    ```


    ---

    SUMMARY TABLE

    `Maybe`
        - Logic: Preference
        - `empty` (The Identity): `Nothing`
        - `x <|> y` behavior: First `Just`, or `Nothing`
    LIST
        - Logic: Collection
        - `empty` (The Identity): `[]`
        - `x <|> y` behavior: Concatenation (`++`)
    PARSER
        - Logic: Choice
        - `empty` (The Identity): `failure`
        - `x <|> y` behavior: Try `x`, if fail, try `y`


    ---





-}

-----   -   --- -   ------  -   -   --  ------  -   ------  -   ----    -   ----
-----   -   --- -   ------  -   -   --  ------  -   ------  -   ----    -   ----

-----   -   --- -   ------  -   -   --  ------  -   ------  -   ----    -   ----