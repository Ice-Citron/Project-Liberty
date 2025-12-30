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
         wWWWw               wWWWw
   vVVVv (___) wWWWw         (___)  vVVVv
   (___)  ~Y~  (___)  vVVVv   ~Y~   (___)
    ~Y~   \|    ~Y~   (___)    |/    ~Y~
    \|   \ |/   \| /  \~Y~/   \|    \ |/
   \\|// \\|// \\|/// \\|//  \\|// \\\|///
   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}