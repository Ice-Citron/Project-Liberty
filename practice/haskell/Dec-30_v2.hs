-- module Main where

import Control.Applicative
import Data.Char (isDigit, isAlpha)

import Data.Functor

-- 1. THE TYPE DEFINITION (The "Reader Monad ish" part)
-- It wraps a function: String -> [(a, String)]
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- 2. BOILERPLATE INSTANCES (To make <|> and <*> work)
instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f x, s') | (x, s') <- p s]

instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser pf) <*> (Parser px) = Parser $ \s -> 
        [ (f x, s'') | (f, s') <- pf s, (x, s'') <- px s' ]

instance Alternative Parser where
    empty = Parser $ \_ -> []
    -- The "Magic" Choice Operator
    (Parser p) <|> (Parser q) = Parser $ \s -> p s ++ q s

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
        -- combine a bunch of things in sequence into a t producing a list.
        < sequenceA :: Applicative t => [t a] -> t [a]                     
        
        -- map the given list and then sequence resulting ts
        < traverse  :: Applicative t => (a -> t b) -> [a] -> t [b]         
-}

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
    0) REMINDER: WHAT A PARSE RESULT MEANS

    `runParser p input :: [(res, rem)]`
            `runParser :: String -> `
    - `res` = what was parsed (type `a`)                <-- RESULT
    - `rem` = the remaining input not consumed yet      <-- REMAINDER
-}


----------      -   ----- -- --         ---------   -   ----- - - -----------

{-
    primarily about FLIPPING THE STRUCTURE INSIDE OUT.

    ---

    1. `sequenceA`: "The Inside-Out Flipper"

    Imagine you have a LIST OF BOXES. You want to swap them so you have a BOX
    CONTAINING A LIST.
    - INPUT: [Maybe Int]        (A list of maybe-integers)
    - OUTPUT: Maybe [Int]       (Maybe a list-of-integers)

    THE RULE:
    - If ALL the boxes inside are valid (e.g., `Just`), it combines their 
      contents into one list inside a `Just`.
    - If EVEN ONE box is empty (`Nothing`), the entire result becomes `Nothing`.
      (It fails fast).

    EXAMPLE: THE GROUP PROJECT 
    Imagine you ask 3 friends for their part of a project.

            Friend 1: Just "Part A"
            Friend 2: Just "Part B"
            Friend 3: Just "Part C"
-}

-- You have a list of their responses:
responses = [Just "Part A", Just "Part B", Just "Part C"]

-- sequenceA flips it into one big result:
ra = sequenceA responses 
-- Result: Just ["Part A", "Part B", "Part C"]


-- The Failure Case: If Friend 2 flakes out (Nothing):
badResponses = [Just "Part A", Nothing, Just "Part C"]

rb = sequenceA badResponses 
-- Result: Nothing
-- (Because one failure ruins the whole sequence)



----------      -   ----- -- --         ---------   -   ----- - - -----------

{-
    2. `traverse`: "Map, then Flip"

    `traverse` is just a shortcut. It's exactly `map` followed by `sequenceA`.

    Use `traverse` when you start with a NORMAL LIST (like raw data) and you 
    want to run a function that might fail (returns a `Maybe`) on every item.


    THE RECIPE:
    1. MAP: Apply the risky function to every item. (Now you have a List of 
       Maybes).
    2. SEQUENCE: Flip it inside out. (Now you have a Maybe List).


    EXAMPLE: THE GROCERY SHOPPER
    You have a list of IDs. You want to look up the item name for each ID. The
    lookup might fail (`Nothing`).
-}

-- The risky function
lookupFood :: Int -> Maybe String
lookupFood 1 = Just "Apple"
lookupFood 2 = Just "Banana"
lookupFood _ = Nothing

-- The raw data
shoppingList = [1, 2]

-- USING TRAVERSE:
-- traverse lookupFood shoppingList
-- Result: `Just ["Apple", "Banana"]`


{-
    WHY IS THIS BETTER THAN `map`? If you used `map`, you would get 
    `[Just "Apple", Just "Banana"]`. You'd be stuck holding a list of boxes.
    `traverse` handles the unwrapping for you, giving you one clean 
    `Just ["Apple", "Banana"]` at the end.


    SUMMARY
    - `sequenceA`: Used when you already have a list of containers 
      (`[Just 1, Just 2]`) and want to flip it (`Just [1, 2]`).
    - `traverse`: Used when you have raw data (`[1, 2]`) and want to apply a
      function that creates containers, then flip it immediately.
-}

----------      -   ----- -- --         ---------   -   ----- - - -----------

----------      -   ----- -- --         ---------   -   ----- - - -----------
{-
               ,~.
              {;@;}
        ,~.  ..`~' . *  ,~.
       {;@;}  . ,~. ** {;@;}
     *..`~'  * {;@;} .  `~'
   ,~.   .. **  `~'  ** . ,~.
  {;@;} * ,~.   * ... *  {;@;}
   `~' ..{;@;} **  ,~. .. `~'
    ,~. . `~'. .. {;@;}   *..  *
   {;@;}  .. ,~. ..`~' **   ,~.
    `~'. ** {;@;}  **   ** {;@;}
      .. * . `~'  ** ,~. ...`~'
       * ~~  ** ... {;@;} .
     .____  ~ *  ~   `~'.~____.
      \ \ \\  | ||| |  / / / /
  ____ \___\\ || | || /,/___/ ____
./___ \_____\\| |||||//______/ ___\.
 \____/  ,___. | | | .___,   \____/
        /     \,--. /`__/ \
       |     \_\   /_      |
        \ ,--, /`_'\ \    /
         `____/  /  \____'
             /  /|\  \\,
            (  (|||\  \ \
            /\  \||\\  \ \
            //\  \|\\)  )
               )  ) \'`'
              `'`'          Valkyrie
------------------------------------------------
-}

{-
    `($>)` is the "REPLACE WITH" operator.

    It takes a container on the left, throws away the values inside it (keeping
    the structure/effects), and replaces them with the value on the right.


    THE VISUAL RULE
    Look at where the arrow points. That is the value you KEEP.
    - `(<$)`: `x <$ container` (Keep `x` on the left).
    - `($>)`: `container $> x` (Keep `x` on the right).


    1. SIMPLE EXAMPLE
    LISTS: It preserves the length (structure) but overwrites the content.
-}
ha = [1, 2, 3, 4] $> "Hello"
-- Result: ["Hello", "Hello", "Hello", "Hello"]


 -- MAYBE: It preserves the Success/Failure status.
hb = Just 10 $> "Winner"
-- Result: Just "Winner"

hc = Nothing $> "Winner"
-- Result: Nothing (Can't put a value in a box that doesn't exist)


----------      -   ----- -- --         ---------   -   ----- - - -----------

{-
    2. THE PARSING USE-CASE (Why you care)

    This is extremely useful when writing parsers for specific keywords. You 
    match the string "true", but you want to return the boolean `True`.
-}

-- Match the string "true", ignore the string itself, return boolean True
parseTrue :: Parser Bool
parseTrue = string "true" $> True

-- Match the string "null", return Nothing
parseNull :: Parser (Maybe a)
parseNull = string "null" $> Nothing







----------      -   ----- -- --         ---------   -   ----- - - -----------
{-
      ,
 ,,   %%   ,   ,
:%%%; ;%%% ;%  %%:
 :%%% %%%% %% %%:
:%%%%% %%%% % %%%:
:%%%%%% %%%% %%%%:
 :%%%%% %%% %%%%:
  :%%%% %% %%%%:
   :%%%%% %%%%:
    :%%%% %%%:
     &%:%&:%&
      &&:&:&
       &&&&
       ;&&;     ;
        ::    ;;;;
        ::   ;;;;;
        ::  ;;;;;;
        :: ;;;;;;
  ;     :: ;;;;;
 ;;     ::;;;;
;;;;    ::;
;;;;;   ::
;;;;;;  ::
 ;;;;;  ::
   ;;;; ::
      ;;::
       ;::
        ::
        ::
        ::
        ::
        ::
        ::
        ::
        :: 
------------------------------------------------
-}

{-
    In OOP, data and methods are glued together. In Haskell, they are completely
    separate.

    Let's fix this mental model by building a parser from scratch without the
    "magic" syntax first, then adding the magic back in.


    1. THE "BOX" ANALOGY (Demystifying `runParser`)

    Imagine a `Parser` is just a physical ENVELOPE. Inside the envelope is a
    piece of paper with INSTRUCTIONS (a function).
    
    - THE TYPE: `Parser a` is the type of Envelope that promises to produce an 
      `a`.
    - THE VALUE: `myParser` is a specific Envelope sitting on your desk.
    - THE UNWRAPPER: `runParser` is a tool (a letter opener) that takes an
      Envelope, opens it, and hands you the Instructions inside so you can 
      execute them.


    THE "MANUAL" WAY (No Magic)

    If Haskell didn't have record syntax (the `{}` stuff), we would write this:
-}


-- 1. Define the Box (The Type)
-- It holds a function that takes a String and gives a list of (result, leftover)
data Parser' a = P' (String -> [(a, String)])

-- 2. Define the Unwrapper (The Accessor)
-- This function takes a Box, opens it, and returns the function inside.
runParser :: Parser a -> (String -> [(a, String)])
runParser (P insideFunction) = insideFunction





----------      -   ----- -- --         ---------   -   ----- - - -----------



----------      -   ----- -- --         ---------   -   ----- - - -----------
----------      -   ----- -- --         ---------   -   ----- - - -----------

--      [TYPE NAME]        [CONSTRUCTOR NAME]
newtype Q_Parser a      =  Q (String -> [(a, String)])

{-
            Type and Constructor...

            `Q_Parser` is the name of the Type (concept), and `P` is the name of
            the CONSTRUCTOR (the function that builds it).

            They live in two different worlds.


            ---

            Summary
            - `Q_Parser`
              * Role: The Type
              * Where it's used: In type signatures: `myFunc :: Parser Int`
            - `Q`
              * Role: The Constructor
              * Where it's used: In code to build or unpack: 
                `val = P (\s -> ...)`
-}

{-
                        1. The Two WorldsHaskell keeps "Types" and "Values" completely separate.The World of Types: This is where Int, Char, String, and Parser live. You only see these after ::.The World of Values: This is where 5, 'c', "hello", and P live. You see these in the actual code execution.When you write:Haskell--      [TYPE NAME]      [CONSTRUCTOR NAME]
                newtype Parser a      =  P (String -> [(a, String)])
                Parser a (Left Side): This is the label you put on the box. It tells the compiler "This variable holds a Parser."P (Right Side): This is the factory or tag used to create the box.2. Can we name them the same?Yes! In fact, experienced Haskell developers often do this to keep things simple.You could have written:Haskellnewtype Parser a = Parser (String -> [(a, String)])
                If you did this:Type: ParserConstructor: ParserWhy did the author use P instead?Authors of tutorials often use different names (like P or MakeParser) to help beginners realize that the Type and the Constructor are two different things, even if they share a name.P is short, so it makes writing the code less cluttered.It distinguishes the wrapper (P) from the concept (Parser).3. Visualizing P as a WrapperThink of P as a function that wraps your raw logic into the Parser type.Haskell-- This is a raw function
                myFunc :: String -> [(Char, String)]
                myFunc inp = ...

                -- This is a syntax error! 
                -- You cannot say myFunc is type "Parser Char" yet.
                -- It is just a function.
                x :: Parser Char
                x = myFunc  -- ERROR!

                -- This works. P wraps the function.
                y :: Parser Char
                y = P myFunc -- CORRECT!
                4. Summary TableCodeRoleWhere it is usedParserThe TypeIn type signatures: myFunc :: Parser IntPThe ConstructorIn code to build or unpack: val = P (\s -> ...)So, P works because it is defined as the unique "tag" that turns a raw function into a Parser. If you changed P to Potato in the data line, you would have to type Potato (\s -> ...) every time you created a parser!
-}



{-
                        You are absolutely right to question this. It feels redundant because, in many ways, it is!

                To prove to you that the name P is completely arbitrary and that the code works exactly the same way regardless of what you call it, here are the examples you asked for (Q, R, S, TOTOT, HELLO).

                1. The "Anything Goes" Examples
                In all these examples, the logic is identical. Only the label on the box changes.

                Example Q (Using Q as the constructor)

                Haskell

                -- Type: Q
                -- Constructor: MakeQ
                data Q = MakeQ Int

                -- Creating one
                myValue :: Q
                myValue = MakeQ 5
                Example R (Using R as the constructor)

                Haskell

                -- Type: R
                -- Constructor: R_Wrapper
                data R = R_Wrapper String

                -- Creating one
                myValue :: R
                myValue = R_Wrapper "hello"
                Example TOTOT (Using TOTOT as the constructor)

                Haskell

                -- Type: UserID
                -- Constructor: TOTOT
                data UserID = TOTOT Int

                -- Creating one
                id :: UserID
                id = TOTOT 101

                -- Unwrapping it (Pattern Matching)
                printId :: UserID -> Int
                printId (TOTOT x) = x  -- Works perfectly
                Example HELLO (Your Parser example) If we renamed P to HELLO in your parser file, it would look like this:

                Haskell

                -- Definition
                newtype Parser a = HELLO (String -> [(a, String)])

                -- Instance
                instance Monad Parser where
                -- We must use 'HELLO' to pack and unpack now
                p >>= f = HELLO (\inp -> 
                    case runParser p inp of
                        [] -> []
                        [(v, out)] -> runParser (f v) out
                    )
                Conclusion: The compiler treats the Constructor purely as a function that tags the data. You can name it Pizza, Box, or P.

                2. Why not just use the original type name?
                You asked: "why not just use the original type name itself... like wont this just make code more complex to maintain?"

                You are actually correct. In professional Haskell "production" code, it is extremely common (and idiomatic) to use the same name for the Type and the Constructor when there is only one constructor (like in a newtype).

                The "Professional" Way (Same Name):

                Haskell

                -- Type is 'Parser', Constructor is ALSO 'Parser'
                newtype Parser a = Parser { runParser :: String -> [(a, String)] }
                Why do tutorials (and your file) use P instead? There is one specific reason: Teaching Clarity.

                When a beginner sees this:

                Haskell

                f :: Parser Int -> Parser Int
                f (Parser p) = Parser p
                It is very easy to get confused: "Wait, is the first Parser the same thing as the second Parser?" (Answer: No. The first is a Type, the second is a Value/Constructor).

                By using P:

                Haskell

                f :: Parser Int -> Parser Int
                f (P p) = P p
                It becomes visually obvious: "I am taking a type Parser, breaking open the box P, and putting it back in a box P."

                3. Does it make maintenance harder?
                No. It actually makes no difference to maintenance.

                Different Names: Easier for humans to distinguish Type vs Value while reading pattern matches.

                Same Names: Cleaner namespace, fewer names to remember.

                Recommendation: Since you are learning, stick with P (or whatever your tutorial uses) so you can mentally separate "The Type" from "The Tag." Once you are comfortable, you will likely switch to using the same name (like Parser) for both in your own projects.
-}



-------------

{-
    2. THE PARSING USE-CASE (Why you care)

    This is extremely useful when writing parsers for specific keywords. You
    match the string "true", but you want to return the boolean `True`.
-}

-- Match the string "true", ignore the string itself, return boolean True
parseTrue' :: Parser' Bool
parseTrue' = string "true" $> True

-- Match the string "null", return Nothing
parseNull' :: Parser' (Maybe a)
parseNull' = string "null" $> Nothing

{-
    You are confused because you are trying to "see" the `True` inside the 
    `Parser`

-}