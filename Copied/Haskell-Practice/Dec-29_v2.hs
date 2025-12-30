
{-
    How `(>>)` Actually Works

        The operator `>>` (pronounced "then") is the SEQUENCING OPERATOR.

        Its logic is: "RUN THE FIRST ACTION. IF IT SUCCEEDS, IGNORE ITS VALUE
        (BUT KEEP ITS SIDE EFFECTS/CONTEXT), AND THEN RUN THE SECOND ACTION."

        It's essentially a shortcut for `>>=` where you don't carte about the 
        argument:

        ```Haskell
        x >> y   ===   x >>= (\_ -> y)
        ```


        THE "MAYBE" LOGIC (THE LANDMIND)

                    ghci> Nothing >> Just 100
                    Nothing
                
                
                For Maybe, the "context" is Success vs. Failure.

                Success (Just): Continue to the next step.

                Failure (Nothing): Abort everything immediately (Short-circuit).

                Let's look at the three scenarios:

                1. Success then Success (What you wanted)

                Haskell

                Just 10 >> Just 200
                -- 1. Look at left: Just 10? Okay, success!
                -- 2. Discard the '10'.
                -- 3. Return the right side: Just 200.
                -- Result: Just 200
                2. Failure on the Left (The Short Circuit)

                Haskell

                Nothing >> Just 200
                -- 1. Look at left: Nothing.
                -- 2. ABORT! Do not even look at the right side.
                -- Result: Nothing
                This is why >> is useful. It checks for failure for you automatically.

                3. Failure on the Right

                Haskell

                Just 10 >> Nothing
                -- 1. Look at left: Just 10? Success!
                -- 2. Proceed to right side.
                -- 3. Right side is Nothing.
                -- Result: Nothing


        - This is why `>>` is useful. It checks for failure for you 
          automatically.

    
    ---

    Why use >> instead of just returning the second value?
    You might ask: "Why write a >> b? Why not just write b?"

    Because a might fail (or have other side effects like printing to screen).

    ```Haskell
    -- In IO Monad
    print "Hello" >> print "World"
    -- Result:
    -- Prints "Hello"
    -- Prints "World"
    -- (If you just wrote 'print "World"', the first print would never happen!)
    ```


    ---

    SUMMARY
    1. THE RULE: `a >> b` means "Run `a` for its effects/checks, throw away the
       result, then run `b`."
    3. FOR MAYBE: It acts as a guard. "if `a` is not Nothing, give me `b`., 
       Otherwise, give me Nothing."
-}

-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----
-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----
-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----

-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----
-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----


{-
                Week 5: Applicatives
                    The Problem: Functor Isn't Enough
                    Last lecture we learned that fmap transforms values inside a structure. But what if you want to combine two structures?
                    haskellmx :: Maybe Int
                    my :: Maybe Int

                    -- I want to add these together and get a Maybe Int
                    -- But fmap only works on ONE structure at a time!
                    With just Functor, you're stuck writing specific functions:
                    haskelladdMaybe :: Maybe Int -> Maybe Int -> Maybe Int
                    addMaybe Nothing  _        = Nothing
                    addMaybe _        Nothing  = Nothing
                    addMaybe (Just x) (Just y) = Just (x + y)
                    And then if you want multiplication, you write another function. And for lists, you write it again. This is tedious.


    ---

    THE INSIGHT: LIFTING FUNCTIONS

    What we really want is to take a regular function like 
    `(+) :: Int -> Int -> Int` and make i work on 
    `Maybe Int -> Maybe Int -> Maybe Int`.

    This is called LIFTING--we lift a function to work "inside" a structure.

    ```Haskell
    liftA2 :: Applicative t => (a -> b -> c) -> t a -> t b -> t c
    ```
 
    `liftA2` takes two-argument functions and makes it work on two wrapped 
    values. Now we can write


    ```Haskell
    addMaybe = liftA2 (+)
    mulMaybe = liftA2 (*)
    addList  = liftA2 (+)  -- same code works for lists!
    ```

    ---


    Let's think about what `liftA2` needs to work.

    Question: If `liftA2` combines two structures, and `fmap` (which we would 
    call `liftA`) transforms one structure without changing its size... can we
    implement `liftA` in terms of `liftA2`?

    ---


    For lists: `liftA2 f xs ys` produces a list of size `length xs * length ys`.

    If `liftA f xs` must produce a list of size `length xs`, then:

    ```Haskell
    length xs * length ys = length xs
    ```

    This only works if `length ys` = 1. So we need a way to create a "size 1"
    structure containing any value:

    ```Haskell
    pure :: Applicative t => a -> t a
    ```

    `pure x` creates the MINIMAL STRUCTURE containing `x`.



    ---

    THE APPLICATIVE CLASS

    ```Haskell
    class Functor t => Applicative t where
      pure  :: a -> t a
      (<*>) :: t (a -> b) -> t a -> t b     --  pronounced "apply"
    ```

    The `(<*>)` operator is the key insight: it applies a WRAPPED FUNCTION to a
    WRAPPED VALUE.

    The relationship between `liftA2` and `(<*>)`:

    ```Haskell
    liftA2 f mx my = pure f <*> mx <*> my
    -- ===
    liftA2 f mx my = f <*> mx <*> my            -- more idiomatic
    ```

    And `(<*>)` can be defined from `liftA2`:

    ```Haskell
    (<*>) = liftA2 ($)
    -- === 
    (<*>) f x = liftA2 ($) f x      <-- intersting, so in this case. both f and x are containerized...
    ```


    ---

    ABOUT `<*>` CHAINs...

        WHAT HAPPENS WITH NOTHING?

        The `Nothing` propagates through/ This is the 
-}



{-
            print (sum (map f xs))
            print $ sum (map f xs)
            print $ sum $ map f xs
-}

{-
              .
             .@.                                    .
             @m@,.                                 .@
            .@m%nm@,.                            .@m@
           .@nvv%vnmm@,.                      .@mn%n@
          .@mnvvv%vvnnmm@,.                .@mmnv%vn@,
          @mmnnvvv%vvvvvnnmm@,.        .@mmnnvvv%vvnm@
          @mmnnvvvvv%vvvvvvnnmm@, ;;;@mmnnvvvvv%vvvnm@,
          `@mmnnvvvvvv%vvvvvnnmmm;;@mmnnvvvvvv%vvvvnmm@
           `@mmmnnvvvvvv%vvvnnmmm;%mmnnvvvvvv%vvvvnnmm@
             `@m%v%v%v%v%v;%;%;%;%;%;%;%%%vv%vvvvnnnmm@
             .,mm@@@@@mm%;;@@m@m@@m@@m@mm;;%%vvvnnnmm@;@,.
          .,@mmmmmmvv%%;;@@vmvvvvvvvvvmvm@@;;%%vvnnm@;%mmm@,
       .,@mmnnvvvvv%%;;@@vvvvv%%%%%%%vvvvmm@@;;%%mm@;%%nnnnm@,
    .,@mnnvv%v%v%v%%;;@mmvvvv%%;*;*;%%vvvvmmm@;;%m;%%v%v%v%vmm@,.
,@mnnvv%v%v%v%v%v%v%;;@@vvvv%%;*;*;*;%%vvvvm@@;;m%%%v%v%v%v%v%vnnm@,
`    `@mnnvv%v%v%v%%;;@mvvvvv%%;;*;;%%vvvmmmm@;;%m;%%v%v%v%vmm@'   '
        `@mmnnvvvvv%%;;@@mvvvv%%%%%%%vvvvmm@@;;%%mm@;%%nnnnm@'
           `@mmmmmmvv%%;;@@mvvvvvvvvvvmmm@@;;%%mmnmm@;%mmm@'
              `mm@@@@@mm%;;@m@@m@m@m@@m@@;;%%vvvvvnmm@;@'
             ,@m%v%v%v%v%v;%;%;%;%;%;%;%;%vv%vvvvvnnmm@
           .@mmnnvvvvvvv%vvvvnnmm%mmnnvvvvvvv%vvvvnnmm@
          .@mmnnvvvvvv%vvvvvvnnmm'`@mmnnvvvvvv%vvvnnmm@
          @mmnnvvvvv%vvvvvvnnmm@':%::`@mmnnvvvv%vvvnm@'
          @mmnnvvv%vvvvvnnmm@'`:::%%:::'`@mmnnvv%vvmm@
          `@mnvvv%vvnnmm@'     `:;%%;:'     `@mvv%vm@'
           `@mnv%vnnm@'          `;%;'         `@n%n@
            `@m%mm@'              ;%;.           `@m@
             @m@'                 `;%;             `@
             `@'                   ;%;.             '
   ,          `                    `;%;
   %,                               ;%;.
   `;%%%%%%%,                       `;%;
     `%%%%%%%%%,                     ;%;.
              ::,                    `;%;
              ::%,                    ;%;
              ::%%,                   ;%;
              ::;%%                  .;%;
              ::;;%%                 ;%;'
              `::;%%                .;%;
               ::;;%%               ;%;'
               `::;%%              .;%;
                ::;;%%             ;%;'
                `::;%%            .;%;
                 ::;;%%           ;%;'
                 `::;%%          .;%;
                  ::;;%%         ;%;'
                  `::;%%        .;%;
                   ::;%%,       ;%;'
                   `::;%%      .;%;
                    ::;%%      ;%;'
                    `::%%     .;%;
                     ::%%     ;%;'
                     `:;%    .;%;
                      :;%   .;%;'
                      :;%  .;%%;
                      `:% .;%%;'
                       `::%%;'
-}



---- -- - ---   -   ------  -   -   -   -   -----   -   -   -   -   -   -
{-
    `($)`--plain function application:

    ```Haskell
    ($) :: (a -> b) -> a -> b
    ($) f x = f x
    ```
-}



{-
    THE KILLER APP: EXPRESSION EVALUATION

    Here's where Applicative shines. Consider evaluating expressions where
    variable lookup might fail:

    ```Haskell
    data Expr = Add Expr Expr | Var String deriving Show

    evalMaybe :: Expr -> [(String, Int)] -> Maybe Int
    evalMaybe (Var v) ctx = lookup v ctx        -- lookup already returns Maybe
    evalMaybe (Add e1 e2) ctx = (+) <$> evalMaybe e1 ctx <*> evalMaybe e2 ctx
    ```

    The second line is beautiful. It says: "Evaluate both subexpressions. If
    both succeed, add them. If either fails, the whole thing fails."

    Without Applicative, you'd write:

    ```Haskell
    evalMaybe (Add e1 e2) ctx = case evalMaybe e1 ctx of
      Nothing -> Nothing
      Just m  -> case evalMaybe e2 ctx of 
        Nothing -> Nothing
        Just n  -> Just (m + n)
    ```

    The Applicative version is not just shorter--it's clearer about the intent.

    ---


    SWITCHING ERROR TYPES FOR FREE

    Here's the magic: if you want error messages instead of just `Nothing`, 
    change `Maybe` to `Either String`:

    ```Haskell
    evalEither :: Expr -> [(String, Int)] -> Either String Int
    evalEither (Var v) ctx = case lookup v ctx of
      Nothing -> Left (v ++ " is out of scope")
      Just n  -> Right n
    evalEither (Add e1 e2) ctx = (+) <$> evalEither e1 ctx <*> evalEither e2 ctx
    ```
            -- btw, `case ... of` IS PATTERN MATHCING

    The Add case is identical! The Applicative abstraction means you can swap
    out error-handling strategies without touching the core logic.


    --- 

    The Limitation: Nested Structures

    Applicatives can't handle everything. What if your combining function also
    produces a wrapped value?

    ...

    Can't handle... need Monads instead



            haskellsafeDiv :: Int -> Int -> Maybe Int
            safeDiv _ 0 = Nothing
            safeDiv m n = Just (div m n)

            liftA2 safeDiv (Just 10) (Just 2) :: Maybe (Maybe Int)
            --                                   = Just (Just 5)

            liftA2 safeDiv (Just 10) (Just 0) :: Maybe (Maybe Int)  
            --                                   = Just Nothing
            We get Maybe (Maybe Int) instead of Maybe Int. Applicative can't flatten this nested structure. For that, you need Monads (next week).

-}


-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----
-- -- - --- -   -   -   --  ------- -   -   ------  -   -   -- -- --        ----


{-
           .-._                        
          : .-.'.                      
         :.'-';`:                      
          :'-' ':                      
          '.__.'                       
             ;                         
   .-.       ;                         
 .'.-.i.    .;                         
 :'-': '.    ;                         
i''-'   :    ;              _.-._      
 '-.__.i.    ;'            : ..-.'.    
        "; ,  ;           : '.''-':    
          ;  .;       ._.-": :"'.'     
         ,',  ; '   ,-'     '=-"       
           ;  ;  i,'-                  
           ',_;_,'                     
       _.-""; ;; ""-._                 
     ."    ,; ;       ".               
    :       ';; ;'     :               
    i-.__...----...__.-i               
     :                :                
      '.            .'                 
        :          :                   
         :        :                    
         :        :                    
         :        :                    
       ,'          '.                  
    ,-'              '-.               
   ;                    ;              
  :                      :             
 :                        :            
 :                        :            
 :                        :            
  :                      :             
   '.                  .'           
     '-.___      ___.-'                
           """"""
-}


{-
    EXAM INSIGHTS AND TRICKS

    1. "Write an Applicative instance for this type"

    You need two things:
    - `pure x`: wrap `x` in the minimal/default structure
    - Either `(<*>)` or `liftA2`: combine two structures

    For `liftA2`, think: "When do I have a successful result? When both inputs are
    successful." Handle failure cases first.


    2. "Trace through this applicative expression"

    Always go left to right:
    - First `<$>` applies function to first argument, result is wrapped function
    - Each `<*>` applies wrapped function to next wrapped value
    - Track the type at each step--it tells you what's happening


    3. "What's the difference between Functor and Applicative?"

    - Functor: transform values inside ONE structure (can't change shape)
    - Applicative: COMBINE multiple structures (can change shape--lists get 
      bigger)


    4. The `pure f <*> x = f <$> x` law

    This is tested often. It means: wrapping a function in `pure` then applying
    it is the same as just `fmap`ing it. Use this to simplify expression.


    5. Recognize the pattern

    When you see `f <$> x <*> y <*> z`, mentally read it as "apply f to the 
    contents of x, y and z, handling the structure automatically."


    ```Haskell
    liftA2 f mx my
    -- ===
    pure f <*> mx <*> my
    -- === 
    f <$> mx <*> my


    -- These are all the same function:
    fmap, (<$>)               -- just different names/contexts
    ```
-}

instance Applicative Maybe where
  pure x = Just x       -- minimal structure is Just

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _        _        = Nothing        


instance Applicative [] where
  pure x = [x]          -- minimal structure is singleton List

  (<*>) fs xs = [f x | f <- fs, x <- xs]  -- every f applied to every x

 
instance Applicative (Either e) where
  pure x = Right x      -- success is the minimal structure

  liftA2 :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
  liftA2 f (Right x) (Right y) = Right (f x y)
  liftA2 _ (Left  e)  _        = Left e     -- first error wins
  liftA2 _  _        (Left  e) = Left e