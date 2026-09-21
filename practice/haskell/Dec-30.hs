import Data.Char
import Control.Applicative

{-
    1) WHY THE RESULT IS A LIST AT ALL

    With
-}
newtype Parser a = Parser (String -> [(a, String)])

-- The Primtive: `satisfy`
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser eat
  where eat (c:cs) | f c = [(c, cs)]        -- consume c if (f c) is true
        eat _            = []               -- otherwise fail

digit :: Parser Char
digit = satisfy isDigit

parse :: (Parser a) -> String -> [(a, String)]
parse (Parser p) cs = p cs

{-
    the list means: "ALL POSSIBLE PARSES".
    - `[]` = failure (no parses)
    - `[(v, rest)]` = one successful parse
    - `[(v1, rest1), (v2, rest2), ...]` = MULTIPLE successful parses (ambiguity 
      / nondeterminism)

    This is not about `Char -> Bool` specifically; it's about the choice to
    represent results as a list.


    ---

    2) "DETERMINISTIC" vs "NON-DETERMINISTIC"

    A parser is DETERMINISTIC (in the list-of-result sense) if for every input 
    `s` it returns AT MOST ONE result:

    ```Haskell
    length (parse p s) <= 1
    ```

    Your `digit` is deterministic because `satisfy` only ever returns `[]` or a
    singleton list:

    ```Haskell
    satisfy :: (Char -> Bool) -> String -> [(a, String)]
    satisfy f = Parser $ \case ->
      (c, cs) -> [(c, cs)]
      _       -> []
    ```

    There is no way for it to produce two different outputs on the same input.

    A parser is non-deterministic if on some input it can return TWO OR MORE
    RESULTS.


    ---

    3) WHERE DO AMBIGUITY/BACKTRACKING COME FROM?

    They come from COMBINATORS that combine parsers, especially CHOICE.

    A typical `Alternative` choice for this representation is:

    ```
    empty :: Parser a
    empty = Parser $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p $ Parser q = Parser $ \s -> p s ++ q s
    ```

    That definition literally says: "run both on the same input; keep all 
    results."


    EXAMPLE: EXPLICIT AMBIGUITY
    ```Haskell
    p :: Parser Int
    p = (digit *> pure 1) <|> (digit *> pure 2)

    parse p "7x"    ==   [(1, "x"), (2, "x")]       <-- 2 elements inside here because "7x" is [Char]... which means `(++)` in `(<|>)` definition for `Parser p <|> Parser q` is hence used
    ```

    Same input, two valid parses --> NON-DETERMINISTIC


-}


------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----
------  --  -----   -   ------  -   --- -   ------  -   -----       --  -----

{-

    The key theme here is "WHO CLEANS UP THE MESS WHEN A PARSER FAILS?"


    1. HOW BACKTRACKING WORKS "FOR FREE" (The `lex` Example)

    The text is trying to tell you that your simple parser is SAFE. It doesn't
    "break" the input string when it fails halfway through.

    THE SCENARIO: You have a parser `p = kwLet <|> ident`
    - `kwLet`: Expects the exact string `"let`.
    - `ident`: Expects any variable name (letters).
    - INPUT: `"lex`


    THE STEP-BY-STEP TRACE (THE "BACKTRACKING"):
    1. Attempt 1 (Left Side): Run `kwLet` on `"lex"`.
        - Matches 

-}




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
    For a PROGRAMMING LANGUAGE PARSER (like the one you are likely building for
    your exam), the ideal world is:
    1. SUCCESS: You get exactly ONE result (a singleton list `[(result, rest)]`)
       .
    2. FAILURE: You get ZERO results (an empty list `[]`).

    Getting TWO results (like `[(1, "x"), (2, "x")])` is technically "valid" 
    Haskell code, but for a compiler, it counts as a LOGIC ERROR (ambiguity).


    SO... WHY DO WE USE LISTS `[]` AT ALL?

    You might wonder: "If we only want 1 result, why are we using a List? Why
    not just return the result directly?" 

    We use lists because they give us a very clever way to handle FAILURE 
    without crashing the program.

    List Output:
    - `[]`
        * PARSE ERROR. (e.g., expected a digit but saw 'a')
    - `[x]`
        * SUCCESS. (Deterministic. That's the goal)
    - `[x, y, ...]`
        * AMBIGUITY. (You wrote a bad grammer. Fix your code).


    HOW TO FIX THE "TWO ANSWERS" PROBLEM?

    To ensure you always get a singleton lsit (1 input -> 1 output), you must
    design your `<|>` choices so they are MUTUALLY EXCLUSIVE.

    BAD (AMBIGUOUS / NON-DETERMINISTIC):
    - "Parse a Digit" <|> "Parse a Number"
    - Input "1" matches both! You get two results.

    GOOD (DETERMINISTIC):
    - "Parse a Letter" <|> "Parse a Digit"
    - Input can never be both. You will only ever get one result (or none).


    WHEN IS "TWO ANSWERS" ACTUALLY USEFUL?

    Just so you know why this feature exists: It's used in NLP.

                        Just so you know why this feature exists: It is used in Natural Language Processing (NLP).Sentence: "I saw the man with the telescope."Parser Output 1: (I saw (the man (with the telescope))) $\to$ The man was holding the telescope.Parser Output 2: ((I saw (the man)) (with the telescope)) $\to$ I used a telescope to see the man.In NLP, you want both results so the AI can decide which one makes sense contextually. But for your Haskell exam? Stick to the singleton.


        ----------

                            The "Problem" (Why not?)
                        Now, you turn to the VIP guest and say "Here is your lunch." The VIP asks: "Which one is the lunch?"

                        If this is a Search Engine (e.g., "Find all files named 'notes'"), returning [Pizza, Sushi] is PERFECT. You want all options!

                        If this is a Compiler (e.g., "Parse this specific line of code"), returning [Pizza, Sushi] is a DISASTER.

                        Real Code Example: Why Ambiguity Breaks Things
                        Imagine you are writing a parser for a calculator. You type 1 - 2 - 3.

                        A Non-Deterministic parser (<|>) using lists might return two valid parse trees because it doesn't know if it should group left or right:

                        Parse A: (1 - 2) - 3 = -4

                        Parse B: 1 - (2 - 3) = 2

                        If your parser returns [ParseA, ParseB], your calculator now has two different answers for the same math problem. It doesn't know which one to print.

                        So, is it "Wrong"?
                        In Logic Programming (Prolog) or Regex: No! It is a feature. We want to find all matches.

                        In Compilers/Interpreters: Yes! It is a bug called Ambiguity. A programming language must have exactly one meaning for a line of code.

                        Summary
                        You asked: "Why not use the robot that brings back both?"

                        Answer: Because sometimes you only have one mouth to feed. If the next function in your pipeline expects a single Integer Int, but your parser hands it a list of two Integers [Int, Int], your program logic falls apart because it no longer knows which reality is the "true" one.


-}


------  --  ------  -   -----   -   ---         ----------  -   --  -   ------

{-
    1. `empty` (THE "UNIVERSAL FAILURE")

    Just like `0` os the identity for addition (`x + 0 = x`), `empty` is the 
    identity for the `<|>` operator.
    - LOGIC: It represents a computation that has FAILED and produced no 
      results.
    - FOR LISTS: `empty = []`
    - FOR MAYBE: `empty = Nothing`
    - FOR PARSERS: `empty` is a parser that always fails immediately, consuming
      no input.

    
    EXAMPLE USE CASE: GUARDING 
        Imagine you want to parse a number, but only if it is even.
    ```Haskell
    evenDigit :: Parser Int
    evenDigit = do
      n <- digitInt     -- Assume this parses a digit like 4
      if even n
        then pure n     -- SUCCESS! Return it.
        else empty      -- FAIL! (triggers backtracking if inside a `(<|>)`)
    ```


    ---

    2. `many` (ZERO OR MORE)

    This is exactly like the `*` in Regex (Kleene Star). It runs a parser 
    REPEATEDLY until it fails, and collects the results into a list.
    - LOGIC: "Try to get one. If you can, keep going. If you can't, stop and
      return what you have so far."
    - IMPORTANT: `many` NEVER FAILS. If it finds nothing, it just succeeds with
      an empty list `[]`.

    EXAMPLE USE CASE: OPTIONAL WHITESPACE 
    You want to parse a word, but thee might be spaces before it. Or maybe not.
    ```Haskell
    -- Parse zero or more spaces
    spaces :: Parser String
    spaces = many (char ' ')

        -- Input: "   hi" -> Result: "   " (succeeds)
        -- Input: "hi"    -> Result: ""    (succeeds with empty list)
    ```


    ----

    3. `some` (ONE OR MORE)

    This is exactly like the `+` in Regex. It requires the parser to suceed AT
    LEAST ONCE.
    - LOGIC: "Get one. Then get `many` more."
    - IMPORTANT: If the first attempt fails, `some` FAILS COMPLETELY.

    EXAMPLE USE CASE: INTEGERS
    A number msut have at least one digit. An empty string `""` is not a number.

    ```Haskell
    -- Parse one or more digits
    numberString :: Parser String
    numberString = some digit

        -- Input: "123" -> Result: "123"
        -- Input: "abc" -> Result: FAILURE (empty)
    ```


    ---

    THE "MAGIC" RECURSIVE DEFINITIONS

    This is the part that usually comes up in exam questions asking you to
    implement them manually. `some` and `many` are defined mutually recursively
    using `<|>`
-}

-- 1. "some" means: Do 'v' once, then do 'many v'
some' :: Alternative f => f a -> f [a]
some' v = (:) <$> v <*> many' v

many' :: Alternative f => f a -> f [a]
many' v = some' v <|> pure []


{-
    1. Call `many`: Tries `some`.
    2. Call `some`: Tries to read 'a'. Success! Now calls `many` again.
    3. Call `many`: Tries `some`.
    4. Call `some`: Tries to read 'b'. Success! Now calls `many` again.
    5. Call `many`: Tries `some`.
    6. Call `some`: Tries to read next char. FAIL (end of string).
    7. Back to `many`: `some` failed, so `<|> pure []` kicks in. Returns `[]`/
    8. Result Unwinds: `['a', 'b']`.


    Summary Table
    - `some p`
        * Regex: `p+`
        * Success Condition: Matches 1 or more
        * Behavior on "No match": FAILS (Returns `empty`)
    - `many p`
        * Regex: `p*`
        * Success Condition: Matches 0 or more
        * Behavior on "No Match": Succeeds (Returns `[]`)
    - `empty`
        * Regex: N/A
        * Success Condition: Never succeeds
        * Behavior on "No Match": FAILS immediately


    ---


    2. THE "MAGIC" RECURSION EXPLAINED

    Think of `some` and `many` as two workers having a conversation. They pass
    the job back and forth until the input runs out.
    - `some v` says: "I promise to grab AT LEAST ONE `v`. After I grab it, I'll
      ask `many` to get the rest." 
    - `many v` says: "I'll try to ask `some` to do the work. But if `some` fails
      (because there are no more `v`s), I'll just return an empty list `[]` and 
      call it a day."


    ...

            -- list comprehension qualifiers are processed left-to-right
                -- we also have the case of `++` because we want to treat the 
                -- output list as a "LIST OF SUCCESSES" 
-}

-- 1. THE TYPE DEFINITION (The "Reader Monad ish" part)
-- It wraps a function: String -> [(a, String)]
newtype Parser' a = Parser' { runParser :: String -> [(a, String)] }

-- 2. BOILERPLATE INSTANCES (To make <|> and <*> work)
instance Functor Parser' where
  fmap f (Parser' p) = Parser' $ \s -> [(f x, s') | (x, s') <- p s]

instance Applicative Parser' where
  pure :: a -> Parser' a
  pure x = Parser' $ \s -> [(x, s)]
  
  (<*>) :: Parser' (a -> b) -> Parser' a -> Parser' b
  (<*>) (Parser' pf) (Parser' px) = Parser' $ \s ->
    [ (f x, s'') | (f, s') <- pf s, (x, s'') <- px s']











{-
                ..ooo.
             .888888888.
             88"P""T"T888 8o
         o8o 8.8"8 88o."8o 8o
        88 . o88o8 8 88."8 88P"o
       88 o8 88 oo.8 888 8 888 88
       88 88 88o888" 88"  o888 88
       88."8o."T88P.88". 88888 88
       888."888."88P".o8 8888 888
       "888o"8888oo8888 o888 o8P"
        "8888.""888P"P.888".88P
         "88888ooo  888P".o888
           ""8P"".oooooo8888P
  .oo888ooo.    8888NICK8P8
o88888"888"88o.  "8888"".88   .oo888oo..
 8888" "88 88888.       88".o88888888"888.
 "8888o.""o 88"88o.    o8".888"888"88 "88P
  T888C.oo. "8."8"8   o8"o888 o88" ".=888"
   88888888o "8 8 8  .8 .8"88 8"".o888o8P
    "8888C.o8o  8 8  8" 8 o" ...o"""8888
      "88888888 " 8 .8  8   88888888888"
        "8888888o  .8o=" o8o..o(8oo88"
            "888" 88"    888888888""
                o8P       "888"""
          ...oo88
 "8oo...oo888"" 
-}


{-
    This is the hardest conceptual leap in writing parsers: moving from 
    "functions that eat characters" to "abstract machines that pass state 
    around".

    - The flexibility of `a`
    - The "list-like" behavior (`++`)
    - The mystery of the changing `s`


    1. The Power of `a` in `(a, String)`

        "Does it get more complex? Is there no constraints to what `a` can be?"

    Yes. There are no constraints.

    In `(a, String)`, the `String` is the FUEL (the leftover text), and the `a`
    if the PRIZE (what you managed to extract).

    When you first start (like `item` or `char`), the prize is just a `Char`.
    - Result: `(Char, String)`

    But as you combine parsers, `a` evolves into whatever complex data structure
    you are building.
    - IF YOU PARSE A NUMBER: `a` becomes `Int`.
      - Result: `(123, " + 5")`
    - IF YOU PARSE A QUOTED WORD: `a` becomess `String` (which is `[Char]`).
      - Result: `("hello", " world")`
    - IF YOU PARSE A JSON OBJECT: `a` becomes a custom `JsonObject` struct.
    - IF YOU PARSE A FUNCTION: `a` can even be a FUNCTION (this happens in
      `Applicative`!)

    So, `a` is just a placeholder for "The thing I successfully parsed."


    ---

    2. WHY DOES IT FEEL LIKE LISTS? `(++)`

        "we seem to be building things for list given the use of `++`"

    You are likely looking at the definition for the ALTERNATIVE instance 
    (often written as `(<|>)` or `mplus`), or the basic definition of the Parser
    type itself.

    In many Haskell parser tutorials (like the famous Graham Hutton ones), a
    Parser is defined as returning a LIST OF SUCCESSES:

    ```Haskell
    -- A parser takes a String and returns a LIST of possible (result, leftover)
    -- pairs
    newtype Parser a = Parser (String -> [(a, String)])
    ```

    We use a list `[]` not because we are necessarily parsing a list of items,
    but to handle AMBIGUITY or FAILURE.
    - EMPTY LIST `[]`: The parser failed. It found nothing.
    - LIST WITH ONE ITEM `[(res, rem)]`: The parser succeeded exactly once.
    - LIST WITH MULTIPLE ITEMS: The parser found multiple valid ways to parse
      the input (ambiguity).


                -- `res` stands for result // whilst `rem` stands for remainder

    When you see `++` in an instance like this:
    ```Haskell
    -- The "OR" operator
    instance Alternative Parser where
      p <|> q = Parser (\inp -> (parse p inp) ++ (parse q inp))
    ```

    It means: "Try parser `p`. Then try parser `q`. Combine (`++`) all the 
    successful results from both."



    ---

    3. THE MYSTERY OF `s`, `s'` and `s''`

        "do they each just mean rest of???"

    Yes! This is the most crucial part to visualise.

    In Haskell (which has no mutable variabels), we cannot just say 
    `input = input.consume()`. We have to manually pass the "leftover" string
    from one step to the next.

    Think of it as a RELAY RACE. The `String` is the baton.
    - `s` (The Starting Line): This is the fresh, untouched input string passed
      into the big function.
    - `s'` (The First Handoff): This is the string after the first parser has 
      taken a bite out of it.
    - `s''` (The Second Handoff): This is the string after the second parser has
      taken a bite out of the leftovers from the first.



    VISUALISING THE CHAIN (Monad Instance)

    Let's look at a typical `>>=` (bind) implementation. This is what allows you
    to run one parser after another.

    ```Haskell
    -- The logic: Run parser p, get result v, then run function f with v
    p >>= f = Parser (\s -> concat [parse (f v) s' | (v, s') <- parse p s])
    ```
            -- newtype Parser a = Parser { runParser :: String -> [(a, String)] }
            --
            -- parse :: Parser a -> String -> [(a, String)]
            -- parse p input = runParser p input




-}



{-

                        .od88bo.
                      dP       Yb
                     dP         8
             d88b   dP   d888b  8
             Y888  d8    88888 .8
               88 d88    Y88888P
               8P 888     Y888P
               8  888
               8  888           d8b
               8b 888b         d88P     .d888b
               Y8 8888        d88P     d88888P
                8 8888b       88P     .88P'
                Y888888b      8P      d8P
                 8888888b.    8       8'
                 Y88888888b   Yb      8
                  888888888b   8      8
           d8b    Y888888888b  Yb     8   dP
           888b    Y8888888888  8     8  dP
           Y888b    Y888888888b Yb    8  P
            Y888     Y888888888       8   d888b
             Y88b.    `Y888888P          d88888b.
              `Y88b.    Y8888P .d888b   d88888888b
       .d888b   `Y88b.       d8888888   8888888888
      d888888b      `Y88b   .88888888b d888888888P
      888P'                d8888888888 888888888P
      888                  88888888888 8888888P .d8888b
      888                  Y8888888888 888888P d8888888b
      Y88      d88888888b    Y8888888888888888888888888P
       88     d888P             8888888 888888P      YP
      d88b    Y8P       d88888888b Y888  888P .d8b
      Y888b           .d88888888888b  8  88P d8888b
        Y88b         d88888888888888b  888888888P'
         Y88b        88888888888888888 888888888.
          `888b      Y8888888888P 888  8888888888b
          d8888b      `Y8888888P d88P  8888888888P
         o8888888b      Y8888P  d8P   d888888888P
     b .d888888888b      `Y8P  d88   d8888888P'
     Y88888888888888b         d888  d8P Y888P  db
      Y88888888888888b        8888  8         888b.
          `8888888888888b     Y888             8888b
           Y88888P'`Y8888      Y88b            `8888
                       `Y8b                      888b
                         `8b                     Y888b
           db        d8888888b                    Y888
         d8888b      Y88888888b                    Y88b
  8     d888888       Y888888888b      8888b.       888b
  8.   d8888888            `Y8888       `Y888b.     Y888
  `8b d888888888888888888b     `Yb          `Y88b    888
    Y8888888P           Y88b     Yb           888b   Y88b
      888P                 88b    Yb          8888b   Y88
     d888                  Y88b    8          Y8888b   88
    d8888                    Y88b  Yb          88888b  88
   d88888                     888   8          Y88888  88b
  d888888b                    888b  8    b       `Y88b Y88
  888888888b       .d8b       8888  8    Yb.       `Y8  88
  8888888888b     d88888b     8888  8     88b        8  88
 d88888888888    d8888888.    8888  8     Y88b       8b 88
.8 Y88888  YP   .888888888   .8888 dP      Y88       Y8 88
d8   8888       888888888'  d8888888        88   8    8 88
Y8   Y888b      `88888888 d88888888P        88b d8    8 8P
`8b     Y88b     `8888888888888888P         888888    8 8
 Y8      Y8P      `88888888888888P          888888    8 8
 `88b               Y8888888888P           d888888   d888
  8888b.             `Y888888P'           d888888P    Y8P
  Y888888888888b                          888P'        8
   8888888888888b                       .d888         dP
   Y8888888888888                      d88888.       8P
    Y888888888888b.                  .d88888888P    dP
     Y888888888888888P            .d88888888888   d8P
      Y8888888888888P            d8888888888888b d8P
       `Y8b Y888P'      Y88888P 88888888888888888P'
         Y8b.            .8888.  `Y888888888888888b
          `Y888b.   .d8888888888P    Y888888P'
            Y8888888888888888888.     Y8888P
             Y8888888888888888888b.
              Y88888888888888888888b
               Y8888888888888888888P
                Y88888888888888888
                 88888888888888888
                 8888888888888888P
                 8888888888888'   .d88888b.
                 Y8888888888888888888888888b
                  888888888'`888888888888888b
                  Y88888888  88888888888888.
                   Y88P Y8P  8888888888888888b
                    `8  `8'  88888888888888888b.
                             8888888888888888888888P
                             Y888888888888888888888
                              Y8888888888'      `Y8
                               Y888888888b        8
   VK                           Y88888888888b      8b
                                 Y8888P Y8888b.    Y8
                                  Y88P   8888888b   8
                                   YP    Y8888888b  8b
                                          88888888b Y8
                                          88888888P  8
                      d88b   d888b        88888888   8
                   .d8888P  d88888.       88888888   8
                  d88888P  d8888888       88888888b  8
                  888P'    8888P          88888888P  8
                  8P'     d8888          d88P' `YP  d8
                 .8       Y8888         d88P        8P
                 d8        88888b.   .d8888         8
                 Y8         Y8888888888888P        dP
                 `8          `Y8888888888P        dP
                  8b.          `Y888888P'       .d8
                  8888b          Y888P'      d88888
                  88888b                    d888888
                  Y88888                   d8888888
                   `Y888b..d8b        db   8888888P
                        Y888888b. .d888888888888P'
                         Y88888888888888888
                          Y8888P' `Y888888P
                                    Y8888P
------------------------------------------------
-}