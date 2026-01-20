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
    You are confused because you are looking for the "eatings" inside the result
    of `sequenceA`, but the eating happens inside the IMPLEMENTATION OF `<*>`
    (APPLY), which `sequenceA` uses internally.


    1. `sequenceA` doesn't just "swap containers"

    For simple data like `[Maybe Int]`, yes, it looks like swapping. But for
    ACTIONS (like `Parser` or `IO`), `sequenceA` means "Run them all in order."

    The definition of `sequenceA` for a list is basically this:


            -- OFFICIAL TYPE: sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
    ```Haskell
    sequenceA :: [Parser Char] -> Parser [Char]
    sequenceA [p1, p2, p3] = 
      (:) <$> p1 <*> (sequenceA [p2, p3])                       <-- functor here is Parser... and it's basically ((:) Char [Char]) ... or ((:) (Parser Char) (Parser [Char])) <-- don't forget... type `Parser Char` and `Parser [Char]` by themselves are functions too of type (String -> (Char, String)) and (String -> ([Char], String))
    ```

    It chains them together using `<*>`

    WHERE IS THE "EATING"? It is hidden inside that `<*>` operator!

    Recall the `Applicative` instance for `Parser`:
        ```Haskell
        instance Applicative Parser where
        pure x = Parser $ \s -> [(x, s)]
        (Parser pf) <*> (Parser px) = Parser $ \s -> 
            [ (f x, s'') | (f, s') <- pf s,         -- RUN the first parser (Eating Happens here!)
                           (x, s'') <- px s' ]      -- RUN the second parser (Eating happens here!)


                           ^ with regards to f, x... how interesting... so pf :: String -> [( (a -> b), String)]
                                                                           and
                                                                           px :: String -> [( a, String)]

                                                                           but what's interesting is that ultimately
                                                                           ... everything appears to be used!!!
        ```


----------------|--->
            (<*>) :: Parser (a -> b) -> Parser a -> Parser b
            (Parser pf) <*> (Parser px) = Parser $ \s -> 
            [ (f x, s'') | (f, s') <- pf s,     -- RUN the first parser (Eating happens here!)
                           (x, s'') <- px s' ]  -- RUN the second aprser (Eating happens here!)

                                        ^^ `px` HERE is NOT a Parer. `px` is the INTERNAL FUNCTION that the Parser was hiding.

    
    Every time `sequenceA` uses `<*>`, it is hard-coding a command to "Run P1,
    then take the leftover string `s'`"

    
-}


{-
        1. understand example input that leads to pf px... 
                This is the final piece of the puzzle. You are confused because you are imagining a "List of Functions" implies there are thousands of functions flying around.
            
        2. i think whats also interesting is i really also need to drill in my head that when one does the unwrapping in pattern matching of parsers... that both pf and px are actually the function themselves with type (String -> (a, String))...
                It clicks! That is exactly the realization you needed.
                When you write (Parser pf), you are stripping away the newtype wrapper.

        3. Continue running through leftover content
                You asked: "i thought that the parsers are supposed to return [(a, String )] ??? may i know where is this even?"

        4. Our inquiry into Parsing in general and how eating works
                You are having a massive "Lightbulb Moment" right now, but you are tripping over the difference between Mapping a List and Mapping a Parser.

        5. Visualising the Chain (Monad Instance)
                Visualizing the Chain (Monad Instance)
                Let's look at a typical >>= (bind) implementation. This is what allows you to run one parser after another.

        6. STATE-MONAD + Claude Monad
                The State Monad is the logical evolution of the Reader Monad.


        EXTENSION TO 1. 
            <-- might add on to this question personally... as in... i want to see hard example of eating here... or maybe it was already done... and it also feels weird that we need to go through all this formality tbf...
-}      