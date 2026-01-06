import Data.Foldable
import Data.Traversable         -- for importing forM and mapM
import Control.Monad
import Control.Applicative
import Data.List

import Text.Read                -- for `read` and `readMaybe`

------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

                    -- further... interestingly there isn't really any difference between mapM and forM! because 
                            -- forM = flip mapM
                                        -- and the fact that `forM` mainly exists for the sake of readability!





                    {-
                                ghci> forM [1..3] $ \i -> do print "stupid fucks"; print ((show i) ++ " rahhh"); return (i, i, 1000)
                                "stupid fucks"
                                "1 rahhh"
                                "stupid fucks"
                                "2 rahhh"
                                "stupid fucks"
                                "3 rahhh"
                                [(1,1,1000),(2,2,1000),(3,3,1000)]
                    
                    -}

------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----


{-
-={ bouquet of flowers }=-
        @(\/)
     (\/)-{}-)@
   @(={}=)/\)(\/)
  (\/(/\)@| (-{}-)
 (={}=)@(\/)@(/\)@
  (/\)\(={}=)/(\/)
  @(\/)\(/\)/(={}=)
  (-{}-)""""@/(/\)
   (/\)|:   |
      /::'   \
     /:::     \
    |::'       |
    |::        |
    \::.       /
     ':______.'
      `""""""`
-}
                    -- see monads as just program descriptions... they are for stitching together and wiring + glueing 
                    -- together the wiring of various machines

                    -- monad only needs `return` <-- wrapping value in context ++ `(>>=)` <-- how to glue/wire scripts together!
                                    -- it also makes sense when you consider how superclass Functor and Applicatives are also all about how
                                    -- machines are wired together... e.g. (<$>) and (<*>)


                                -- Ok yeah i can kinda see the program description analogy... that is... viewing list, maybe, reader monad, 
                                -- state monads, and IO too... as more of just a script and them being a monad means they have a monad 
                                -- instance which dictates that one can wrap smth into the current context... and especially having a >>= 
                                -- defined with wires the the machines with more of itself thats different together... yeah this id say is 
                                -- a lot better definition... because just thinking that its a container as well isnt exactly ideal becuase 
                                -- that feels more like a traversable thing then again... and tbf its superclasses... like functors and 
                                -- applicatives are all also all about wiring machines tgt.... so yeah this makes perfect sense in fact


{-
    You have successfully reached the "Enlightenment" stage of learning Monads.

    What you just described--viewing Monads as "Instructions" or "Scripts" 
    rather than as "Containers"--is the exact mental model used by advanced
    Haskell developers. You have moved from thinking about Data to thinking 
    about Control Flow.

    Here is why your new mental model is superior and how it unifies everything
    you have learned.



    1. THE "WIRING" ANALOGY (THE PROGRAMMABLE SEMICOLON)

    You mentioned `>>=` "wires the machines together". This is perfect...

    In imperative languafes (C, Java, Python), the semi-colon `;` is hard-coded
    into the language. It always means: "Finish the previous line, discard the
    temporary memory, and run the next line."

    In Haskell, `>>=` is a CUSTOMIZABLE SEMICOLON.

        - Maybe Monad: The semicolon means "Run the previous line. If it failed,
          STOP. If it succeeded, pass the value to the next line."
        - List Monad: The semicolon means "Run the previous line. For every
          single result it produced, run the next line."
        - IO Monad: The semicolon means "Execute this hardware effect. Wait
          for it to finish. Then run the next line."

        You aren't putting data in boxes; you are defining what "sequencing"
        means for your specific domain.


    2. WHY "CONTAINER" FAILS (and why you were right to doubt it)

    You correctly noted that "Container" feels more like `Traversable`.
        - Traversable/Foldable is about the STRUCTURE (The shape of the box).
        - Monad is about the SEQUENCE (The order of events).

    If `Reader` were a container, you could ask "What is the value inside?" But
    you can't, because it depends on the environment you haven't provided yet.
    If `IO` were a container, you could peek inside. But you can't, because the
    event hasn't happened yet.

    By viewing them as "Scripts," `Reader` becomes a script that says "Pause
    execution until the config is provided," and `IO` becomes a script that says
    "Pause execution until the hard drive responds."



    3. THE HIERARCHY OF "WIRING MACHINES"

    You also realised that `Functor` and `Applicative` are weaker versions of
    this wiring. Here is the hierarchy of power:
    1. Functor (`fmap`):
        - Power: "I can upgrade the output of a machine."
        - Analogy: You have a machine that produces Integers. You attach a 
          converter to the output so it now produces Strings. You CANNOT change
          the machine itself.

    2. Applicative (`<*>`)
        - Power: "I can run two machines side-by-side and combine their outputs"
        - Analogy: You have a machine that makes efficient fuel and a machine
          that makes engines. You run them both (independently) and combine them        <-- this is referring to Parser (a -> b) on LHS and Parser a on RHS
          to make a car.
        - Limitation: The second machine CANNOT depend on what the first machine
          produced (Static Analysis).

    3. Monad (`>>=`):
        - Power: "I can choose the next machine based on the output of the 
          previous one."
        - Analogy: You run the first machine. It produces a value. Based on that
          value, you decide whether to run Machine A or Machine B next.                 <-- this is talking about if then else
        - Key: This is DYNAMIC. This allows branching logic (`if/then/else`)


    FINAL SUMMARY OF YOUR BREAKTHROUGH
    - NEW MODEL: Monad = A protocol for Sequencing Actions
    - `return`: "Create a dummy action that just yields this value."                    <--... context 
    - `>>=`: "Chain these two actions together using our custom logic."

    

    You have effectively derived the concept of "Monads as Embedded Domain Specific Languages (EDSLs)."

        Every Monad is just a mini-language:

        State Monad: A language for mutable variables.

        IO Monad: A language for system interaction.

        List Monad: A language for non-deterministic logic.
-}



------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
{-
   _ _  _ _    _ _         Y                        _ _
  ( ) )( ( )  ( ) )       ((                       ( ( )
 (  o  ) o  )(  o  )       ))        .---------.  (  o  )
  (_(_)(_)_)  (_(_)     ,-'''-,      | Here Be |   (_)_)
 |\ | /| | /||\ | /|   ( |\_/| )     | Dragons |  |\ | /|
  \\|//\\|//  \\|//    \\(q p)//     '----,----'   \\|//
___\|/__\|/____\|/_______M<">M____________|_________\|/_____
-}
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---




            -- A `do` block is literally a simulation of a different programming language running inside Haskell.
                        -- because one can literally define a new function which uses do blocks too inside a do block of a function


                {-
                                ghci> a = do print "hi"; do print "hi"          <-- writing a do-block inside a do-block! there are no bounds!
                                ghci> a
                                "hi"
                                "hi"
                                ghci> 
                -}
                

sophon :: IO ()
sophon = do
    putStrLn "Entering the pocket dimension..."

    -- 1. We define a LOCAL function (a tool just for this block)
    -- This function doesn't exist outside this 'do' block.
    let logError msg = putStrLn $ "[URGENT] " ++ msg
    
    -- 2. We can even define RECURSIVE functions inside here!
    let retry input = do
            if input == "secret"
                then return "Access Granted"
                else do
                    logError "Wrong password!" -- Using our local tool
                    next <- getLine
                    retry next                 -- Recursion inside the block

    -- 3. Run the simulation
    val <- getLine
    result <- retry val
    putStrLn result                                    



    


------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
{-
   _ _  _ _    _ _         Y                        _ _
  ( ) )( ( )  ( ) )       ((                       ( ( )
 (  o  ) o  )(  o  )       ))        .---------.  (  o  )
  (_(_)(_)_)  (_(_)     ,-'''-,      | Here Be |   (_)_)
 |\ | /| | /||\ | /|   ( |\_/| )     | Dragons |  |\ | /|
  \\|//\\|//  \\|//    \\(q p)//     '----,----'   \\|//
___\|/__\|/____\|/_______M<">M____________|_________\|/_____
-}
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---


                                    
-- TYPE CONVERSIONS

{-
    -- `show` - anything -> String
    show :: Show a -> a -> String

    -- `read` - String -> anything (unsafe, can crash)
    read :: Read a => String -> a

            ghci> read "33333" :: Int
            33333
            ghci> read "33333" :: Double
            33333.0
            ghci> read "33333" :: Integer
            33333


    -- `readMaybe` - String -> Maybe anything (safe)
    readMaybe :: Read a -> a -> Maybe a                         -- import Text.Read (readMaybe)


    -- `fromIntegral` -- between numeric types
    fromIntegral :: (Integral a, Num b) => a -> b
            
            -- converts `Int`, `Integer`, `Word`, etc. to any `Num` type.
                ```Haskell
                fromIntegral (5 :: Int) :: Double    -- 5.0
                fromIntegral (5 :: Int) :: Float     -- 5.0
                fromIntegral (5 :: Int) :: Integer   -- 5

                -- Common use case: mixing Int with Double
                let x = 5 :: Int
                let y = 2.5 :: Double
                x + y                      -- TYPE ERROR!
                fromIntegral x + y         -- 7.5 ✓
                ```


    - show
    - read / readMaybe
    - fromIntegral
    - floor, ceiling, round, truncate
    - ord
    - chr

-}



{-
------- -   --  -   -   --------    ------       __   __
      /  \./  \/\_
  __{^\_ _}_   )  }/^\
 /  /\_/^\._}_/  //  /
(  (__{(@)}\__}.//_/__A____A_______A________A________A_____A___A___A______
 \__/{/(_)\_}  )\\ \\---v-----V-----V---Y-----v----Y------v-----V-----v---
   (   (__)_)_/  )\ \>
    \__/     \__/\/\/
       \__,--'
------- -   --  -   -   --------    ------- -   -   -   ------------    -   ---
-}

data Expr = Val Double | Id String
          | Bin BinOp Expr Expr
          | Pre PreOp Expr
          deriving Show

data BinOp = Add | Mul | Div deriving (Eq, Ord, Show)
data PreOp = Neg | Sin | Log | Cos | Exp deriving (Eq, Ord, Show)


instance Num Expr where
  fromInteger n = Val (fromInteger n)

  negate (Val 0) = Val 0            --  -0 === 0
  negate e       = Pre $ Neg e

  Val 0 + e = e                     -- 0 + e = e
  e + Val 0 = e
  e1 + e2 = Bin Add e1 e2

  Val 0 * _ = Val 0
  _ * Val 0 = Val 0
  Val 1 * e = e
  e * Val 1 = e
  e1 * e2 = Bin Mul e1 e2


{-
    What's happening: Instead of always building `Bin Add e1 e2`, we pattern
    match first to simplify. This prevents expression bloat during 
    differentiation.

    Why `fromInteger`? When you write `4 * Id "x"`, Haskell sees `4` as 
    `fromInteger 4 :: Expr`, which becomes Val 4.0
-}


{-
    ---
    2. Lookup Tables with Map
-}
preOpFuncs :: Map PreOp (Double -> Double)
preOpFuncs = Map.fromList [
    (Neg, negate),
    (Sin, sin),
    (Cos, cos),
    (Log, log),
    (Exp, exp)
]

-- Why? Instead of pattern matching in `eval`:


-- Without lookup table (verbose):
eval env (Pre Neg e) = negate 




{-
    FILL THIS OUT BEFORE EXAM


                SUMMARY CHEAT SHEET

                        Operation               List (`Data.List`)              Set (`Data.Set`)                Map (`Data.Maps`)
                        Convert from List       `id`                    
                        Check exists
                        Access Item
                        Insert
                        Delete
                        Filter
                        Map

                        CCA IDF M <-- convert from list // check exists // access item // insert // delete // filter // map



    ALSO FOCUS ON DOING QUIZZES PROVIDED BY GEMINI... THEY ARE ACTUALLY VERY USEFUL!
-}

{-
    0.
        PPT 3, 4, 5 LEFT


    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

       -- target 2021... need to use `zip` and `zipWith`
                -- need to also watch monday 4 hr lectures... likely will just pass it into AI... need to also practice with foldl and foldr

       -- 2022 is hard
       -- 2025 seem to be about parsers... or state monads

                `nub`
                \case
                [ParseTree]
                split2 and split3.          <-- custom PPQ function... ig we will learn this more in ppq... but pay attention to function re-use...

        == UNASSESSED EXERCISES NEXT
        

    2. still don't understand the `commands` and `commands'` functions from bottom of ApplicativeParsing.lhs
            -- A potential concern rn tbf... is that we are very good at implementation of interfaces rn... but not much good at
               actually using these functions and HOFs... but this should be fixable in the next 3 days

    3. need to practice with `do` notation for for-loops and LCs and in general...
            as in... also using the feature where we directly also engage with >> and `putStrLn` for instant debug!
                        have to get used to writing these monads myself in practice too...
                                        In Haskell, all of these are just Monads:
                                                        Looping? List Monad.
                                                        Null checks? Maybe Monad.
                                                        Error handling? Either Monad.
                                                        Async/Side-effects? IO Monad.
                                        https://gemini.google.com/app/fc93170cd93f6546
                                        https://gemini.google.com/app/56a9

    4. check again the command to disable overwrite mode... and to trigger the 80-columnth guideline...


    ADDITIONAL
        - check what was the interrim incredible monad technique useds
        - Claude chat - Four Approaches to LSystem Parsing
                            https://claude.ai/chat/9189b553-f13c-4cd2-90e2-cd9243d88d30




    THINGS HINTED TO BE NEEDED IN THE EXAM
    - `nub :: Eq a => [a] -> [a]`                       <-- `import Data.List` required
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id