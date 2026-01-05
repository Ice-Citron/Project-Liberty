import Data.Traversable
import Data.Foldable

{-
            .'`. ,'`.
      .---./    u    \,---.
   ___|    \    |    /    |___
  \    `.   \   |   /   .'    /
   \_    `.  \  |  /  .'    _/
 .-' `-._  `.:::::::.'  _.-' `-.
 \       `-;:::::::::;-'       /
  >~------~:::::::::::~------~<
 /      _.-;:::::::::;-._      \
 `-._.-'   .`.::::::'.   `-._.-'
    /    .'  /  |  \  `.    \
   /___.'   /   |   \   `.___\
       |   /    |    \    |
       `--'\   .n.   /`---'
            `.'   `.'          
-}



{-
    ... a crucial concept for writing "real" code.

    Here is the mental model:
    - `type` is a NICKNAME (Just a label; no runtime existence)
    - `data` is a STRUCTURE (A real object in memory)
    - `newType` is a BADGE (A compile=time label that disappears at runtime)


    ---
    1. `type` (the Nickname)

    This creates a TYPE SYNONYM. It is purely for readability. The compiler
    literally performs a "find and replace" before doing anything else/
    
    - Runtime Cost: Zero
    - Safety: None. The compiler treats the alias and the original type as 
      identical.
    - Use Case: Making complex signatures readable.
-}

type Name = String
type Age  = Int

-- This function claims to take a Name, but I can pass ANY String.
greet :: Name -> String
greet n = "Hello " ++ n

-- Usage
ga = greet "Alice"      -- Works
gb = greet "some junk"  -- Works (Compiler doesn't care, Name IS String)

{-
        Note: The standard `String` in Haskell is actually defined as 
        `type String = [Char]`. They are literally the same thing.
-}


------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

{-
    2. `data` (The Structure)

    This creates a completly NEW DATA TYPE (Algebraic Data Type). It introduces 
    new constructors and allocates memory at runtime.

        - Runtime Cost: High (relatively). It creates a "box" in memory with a
          tag telling the runtime which constructor was used.
        - Safety: High. You cannot mix this up with other types.
        - Use Case: When you need multiple fields, multiple constructors (like
          enums), or recursive structures.
-}
-- A brand new type. Not just an Int, not just a String.
data User = User String Int
          | Admin String

-- Usage
val :: User
val = User "Alice" 30



{-
    3. `newtype` (THE BADGE)

    This is the special one. It creates a distinct type (like `data`), but it
    is restricted to EXACTLY ONE CONSTRUCTOR WITH EXACTLY ONE FIELD.

    Because of this restriction, the compiler knows it wraps exactly one thing.
    It enforces the type distinction during COMPILATION, but ERASES the wrapper
    at RUNTIME.
        
        - RUNTIME COST: Zero. It runs exactly as fast as the underlying type.
        - SAFETY: High. The compiler treats it as a distinct type.
        - USE CASE:
            * TYPE SAFETY WITHOUT OVERHEAD: Distinguishing `UserId` from `Int`.
            * TYPECLASS INSTANCES: Giving a type a different behavior (e.g.,
              `Sum` vs `Product` for integers)
-}

newtype UserId = UserId Int

-- At runtime, `myId` is just a raw Int (5).
-- But at compile time, you CANNOT pass a regular Int to a function expecting UserId.
myId :: UserId
myId = UserId 5



------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

{-
    SUMMARY TABLE
    - `type`
        - What is it? An Alias (synonym)
        - Constructors (`|`)? None                              <-- CONSTRUCTORS ARE THE OPTIONS // FIELDS ARE THE ARGUMENTS OF EACH OF THE CONSTRUCTORS            --- https://gemini.google.com/app/56a9307b4b1cbda3
        - Fields? N/A
        - Runtime Cost: Free
        - Distinct Type? No (Interchangeable)

    - `data`
        - What is it? A brand new structure
        - Constructors (`|`)? Many allowed
        - Fields? Many allows (for each constructor)
        - Runtime Cost: Allocation Overhead
        - Distinct Type? Yes

    - `newtype`                                                 <-- it doesn't matter how many type parameters (generics) you have on the left side
        - What is it? A zero-cost wrapper
        - Constructors? EXACTLY ONE
        - Fields? EXACTLY ONE
        - Runtime Cost: Free (erased post-compilation)
        - Distinct Type? Yes
-}




------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----
------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

{-
    THE "WHEN TO USE WHAT?" CHECKLIST

    1. Do you need multiple options (OR)? `(e.g., `True | False`, `Leaf | Node`)`
        - Use `data`            <-- because multiple constructors needed
    2. Do you need multiple fields (AND)? (e.g. A user has a name AND an age)
        - Use `data`
    3. Do you just want to rename a complex type to make code easier to read?
        - Use `type`
    4. Do you want to wrap a single existing type for TYPE SAFETY or TYPECLASSES?
        - Use `newtype`
-}

        {-
                            The "Why not just use..." Examples
                            Q: Why use newtype instead of type?

                            Scenario: You have type Password = String.

                            Problem: You can accidentally pass a plaintext message into a function expecting a Password. The compiler won't catch it because they are both just String.

                            Solution: newtype Password = Password String. Now the compiler screams if you pass a raw String.

                            Q: Why use newtype instead of data?

                            Scenario: You have data UserId = UserId Int.

                            Problem: This wraps the Int in a box. To read it, the computer has to follow a pointer to the box, check the tag, and unbox the Int. This is slow.

                            Solution: newtype UserId = UserId Int. The compiler checks the types, but at runtime, the "box" is deleted. It runs exactly as fast as a raw Int.
        -}


{-
                                    You have perfectly summarized the "Haskell Philosophy" on this topic.

                                                You are 100% correct: If you can use newtype, you should use newtype.
                                                This is actually a standard optimization tip in the Haskell community. If you write a data definition that has only one constructor and one field, the compiler (or linter) will often politely tap you on the shoulder and say: "Hey, you should turn this into a newtype to save memory."
                                                Why it's the "Best of Both Worlds"
                                                You described it as the midpoint between type and data. Here is exactly why that makes it the sweet spot:
                                                It is data at Compile Time (Safety): It prevents "Primitive Obsession." You can't accidentally add a Password string to a Username string, even though they are both just strings underneath. The compiler forces you to be explicit.
                                                It is type at Runtime (Speed): Because the wrapper is erased, there is zero pointer indirection. The CPU sees the raw Int or String. You get the abstraction for free ("Zero-cost abstraction").
-}

        -- Basically, `type` and `data` are extremes of a spectrum... and `newtype` is the perfect middleground between them
        -- ... the best of both worlds! And in practice, one should use `newtype` over `type` and use `newtype` over `data`
        -- whenever they can


------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----
------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----
------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----


------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----
------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

{-
    HASKELL TIC-TAC-TOE
        -- looks like we just need to learn these wide range of custom variables from these Data.* libraries
                    -- uses `replicate` to create n^2 Empty cells.     `[100 | _ <- [1..10]]` === `do _ <- [1..10]; return 100`
                    -- `nub` in Data.List
                            -- very smart... if `nub` returns a singleton `[Taken 0]` or `[Taken X]` (which means that there's
                                no other pieces present... because nub only returns special pieces)... this means the whole line 
                                is that players' markers.
                    -- `elem :: (Foldable t, Eq a) => a -> t a -> Bool`
                    -- `any`, `all`
                    
                    ```Haskell
                    parsePosition :: String -> Maybe Position
                    parsePosition s = case words s of
                        [r, c] -> (,) <$> readMaybe r <*> readMaybe c              -- <-- (,) * <$> * <*> * ... is how one turns 2 elements into tuples!
                        _      -> Nothing                                          -- combines two `Maybe Int` into `Maybe (Int, Int)`... tbf probably could've used liftA2 instead
                    ```

                    -- row-major indexing: ...      `where idx = r * n + c`
                    -- used helper function `showcell`


                    ```Haskell
                    doParseAction :: String -> (String -> Maybe a) -> IO a
                    doParseAction errMsg parser = do
                        input <- getLine
                        case parser input of
                          Just x  -> return x
                          Nothing -> putStr errMsg >> doParseAction errMsg parser       <-- incredible... `>>` and recursion of `doParseAction` used...
                    ```
                    -- Key Technoque: Recursive retry pattern for IO with Maybe Validation
                        -- `getLine :: IO String`
                    
                    ```Haskell
                    takeTurn b p = do
                      putStr $ "Player " ++ show p ++ ", make your move (row col): "
                      doParseAction "Invalid move, try again: " tryParseAndMove
                      where
                        tryParseAndMove s = parsePosition s >>= \pos -> tryMove p pos b
                    ```
                    -- Key Technique: Chains `parsePosition` and `tryMove` with `>>=` (both return Maybe).


                    ```Haskell
                    playGame b p = do
                      prettyPrint b
                      newBoard <- takeTurn b p
                      if gameOver newBoard
                        then prettyPrint newBoard >> putStrLn ("Player " ++ show p ++ " has won!")
                        else playGame newBoard (swap p)
                    ```




                --------
                    DON'T understand
                    - prettyPrint
                        - explain to me what does mapM_ do... show example... and why `(.)` with intersperse?
                        - what does `intersperse` do?

                    - how to convert various types among each other
                        - forgot what does `fromIntegeral`
                        - i remember `show`...



    ---

    `intersperse`: From `Data.List` -- it inserts an element between every element of a list.

    ```Haskell
    intersperse :: a -> [a] -> [a]

    intersperse ' ' "Hello"     -- "H e l l o"
    intersperse 0 [1,2,3,4]     -- [1,0,2,0,3,0,4]
    intersperse ',' ["a","b"]   -- won't work! (type mismatch)
    ```


    --- ---
    -}

{-
    You have excellent intuition connecting `forM` to imperative loops. That is
    exactly why it exists!

    Here is the breakdown of the "Big Four" iteration functions. They are all
    variations of the same idea, just tweaked for convenience.



    1. THE MATRIX OF ITERATION

    You can organize them in a 2x2 grid based on two questions:
        1. Do you want the result? (Yes = standard / No = `_` version)
        2. Which comes first? (The Function or the List?)


                        I want the results          I don't care about the results (Side Effects only)
    Function First          `mapM`                          `mapM_`
    List First              `forM`                          `forM_`



    ---

    2. `fmap` vs. `mapM` (The Crucial Difference

    You asked if `mapM` is under Functor. it is not.
    - `fmap` is for PURE TRANSFORMATIONS (functor).
    - `mapM` is for EFFECTFUL ACTIONS (traversable/monad)

    Think of a list of numbers: `[1, 2, 3]`.


    `fmap`
    - Goal: "I want to change every number to a string."
    - Action: `fmap show [1, 2, 3]`
    - Result: `["1", "2", "3"]`
    - Side Effects: None. Nothing is printed, no database is touched. It just
      calculates.


    `mapM` (The Action Runner)
     - Goal: "I want to PRINT every number to the console."
     - Action: `mapM print [1, 2, 3]`
     - Result: It prints 1, 2, 3 to the screen. It also returns a list of 
       results (usually `[(), (), ()]` because `print` returns unit).
     - Mechanism: It visits `1`, runs the ACTION (Effect) you gave it, waits for
       it to finish, then moves to `2`.


    THE RULE: If your function returns a normal value (like `Int`), use `fmap`.
    If your function returns a Monad (like `IO Int` or `Maybe Int`)


                ghci> mapM (\x -> Just x) [1, 3, 4]
                Just [1,3,4]
                ghci> map (\x -> Just x) [1, 3, 4]
                [Just 1,Just 3,Just 4]

                ghci> mapM print [1, 3, 4]
                1
                3
                4
                [(),(),()]



    ---

    3. WHERE DO THEY LIVE?

    - `mapM` / `forM`: These live in `Data.Traversable`.
        - Why? Because to rebuild the list with the new results, you need to
          traverse the structure and keep its shape.
    
    - `mapM_` / `forM_`: These live in `Data.Foldable`
        - Why? Because you are throwing away the results. You are essentially
          "folding" the list into a single big side-effect and discarding the
          structure.


                            ghci> forM [1..5] $ \i -> (do print i; return (i, "hello world"))
                            1
                            2
                            3
                            4
                            5
                            [(1,"hello world"),(2,"hello world"),(3,"hello world"),(4,"hello world"),(5,"hello world")]

                            ghci> forM_ [1..5] $ \i -> (do print i; return (i, "hello world"))
                            1
                            2
                            3
                            4
                            5


                                    -- ** SUPERCLASSES: Any `Traversable` type is also a `Functor` and a `Foldable`
                                                - `Functor` lets you map over elements independently.
                                                - `Foldable` lets you reduce the structure to a summary value, throwing away the shape.
                                                - `Traversable` combiens both, allowing for effectful computations while maintaining the shape.


        forM  :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
        forM_ :: (Foldable t, Monad m)    => t a -> (a -> m b) -> m () 
        mapM  :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
        mapM  :: (Foldable t, Monad m)    => (a -> m b) -> t a -> m ()



    ---

    4. WHEN TO USE WHICH?

    A. `mapM` vs `forM`

    These two do the EXACT SAME THING. The only difference is the order of 
    arguments.

    - Use `mapM` when you have a short, named function.
-}

-- Clean and readable
m1 = mapM print [1, 2, 3]


-- Use `forM` when you want to write a "Loop Block" (a long lambda). This mimics
-- the `for x in list:` syntax from Python/Java.

-- "Loop" style
m2 = forM [1, 2, 3] $ \i -> do
        putStrLn ("Procesing " ++ show i)                               -- show :: Show a => a -> String
        return (i * 2)




{-
    B. THE UNDERSCORE VERSIONS (`mapM_`, `forM_`)

    Use these when the RETURN VALUE IS USELESS.

    If you use `mapM print [1, 2, 3]`, Haskell will return `[(), (), ()]` at the
    end. That's noise. If you just want the printing (the side effect), use
    the underscore version.
    
    - `forM_`: The ultimate "For Loop". 
-}
launchMissiles :: Int -> IO ()
launchMissiles n = print $ "WHAT THE FUCK IS A KILOMETER. MISSILE " ++ (show n) ++ " LAUNCHED!" 

m3 = forM_ [1, 2, 3] $ \i -> do
    putStrLn "Doing work..."                -- This block is just like a Python/C loop body
    launchMissiles i





------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----
{-
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

                            -- Personally did also had a very interesting thought experiment just now... that is... given how 
                            -- mx (>>=) f   ===   concatMap f mx        ... we asked and yes... it's entirely possible to write
                            -- do-block styled code with concatMap alone! as we can have the loopy stuff from the inside of `f`
                                        -- though even then, usually... we would prefer to instead still write things using do-block notation despite both are equivalent... due to readability and idiomaticity...


{-
    ... the friction you are feeling comes from the word "Container."
    
    
    THE "CONTAINER" TRAP

    When we learn Monads, we almost always start with `List`, `Maybe` or `Tree`.
    These are literal containers.
    - `Maybe Int` contains an integer (or nothing).
    - `[Int]` contains integers.

    So naturally, your brain builds the model: Monad = Fancy Box.


    Then you meet `IO Int`.
    - Does it "contain" an integer? Not really.
    - The integer doesn't exist yet! It might come from the user typing on
      a keyboard 5 minutes from now.


    
    The Real Definition: "Computations" (Recipe)

    To understand `IO`, you have to stop thinking of Monads as "Boxes of Data" 
    and start thinking of them as "Recipes for Values."

    - `Maybe Int`: A recipe that might produce an Int.
    - `Reader r Int`: A recipe that needs an environment `r` to produce an Int.
    - `IO Int`: A recipe that INTERACTS WITH THE REAL WORLD to produce an Int.


    The `IO` Monad is a DESCRIPTION OF AN ACTION. It is not the action itself.
    Think of `IO String` not as a "Box containing a String", but as a "Slip of
    paper with instructions on how to get a String from the user."



                            ghci> return 5 :: IO Int
                            5
                            ghci> return 5 :: Maybe Int
                            Just 5


    ---
    Why IO fits the Pattern

    A Monad only needs to satisfy two main things:

    1. `return`: Can you wrap a value into the context?
        - `return 5 :: IO Int`.
        - Meaning: "This is a recipe that does nothing special, just immediately
          hands you the number 5."

    2. `>>=` (bind). Can you chain them?
        - Meaning: "Run the first recipe. Take the result it produces. Use that
          result to create the next recipe. Then run that recipe."


    
                The "State Monad" Secret
                    If it helps, you can actually peek behind the curtain. Conceptually, IO is defined almost exactly like the State Monad you mentioned earlier!

                    The "State" it passes around is the entire Real World.

                    Haskell

                    -- Conceptual definition of IO (simplified)
                    -- RealWorld is a magic token representing the state of the universe.
                    newtype IO a = IO (RealWorld -> (RealWorld, a))
                    It takes the current state of the world.

                    It performs a side effect (changing the world state).

                    It returns the new state of the world and a value a.

                    So in a very strange, abstract way, IO is a container. It contains a function that transitions the universe from one state to the next.


    SUMMARY
    - CONTAINER VIEW (LIMITED): Monads hold data. (Works for List, Maybe, Tree).
    - COMPUTATION VIEW (Universal): Monads are recipes that produce a value in a
      context.
      * Context = "Non-determinism" (List)
      * Context = "Failure" (Maybe)
      * Context = "External Interaction" (IO)


    Once you view `IO` as a "Recipe" or a "Program Description" (that glues and 
    wires different machines' wirings together!) rather than a box, it first 
    perfectly. You are building a script, and `>>=` iks the glue that tapes
    the lines of the scripts together.
    
    -} 



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
{-
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








{-
    0.
        PPT 3, 4, 5 LEFT


    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

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