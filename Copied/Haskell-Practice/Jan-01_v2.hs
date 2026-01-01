

{-
    2. HOW `seq`, `!` AND `deepseq` DIFFER HERE

    Here is how those three operators would behave inside that `go` loop:


    A. `seq` (The Standard strictness)

    `seq` tells Haskell: "Make sure value A is evaluated (to WHNF) before you
    return value B."

    ```Haskell
    -- Same thing as the Bang Pattern version, but using standard Haskell 98
    go acc = do
      x <- p
      let newAcc = x : acc
      -- "Evaluate newAcc slightly, THEN recurse"
      newAcc `seq` go newAcc
    ```

    EFFECT: It ensures the list structure (the "cons cells") exists in memory
    before moving to the next step. It prevents a thunk of `(:)` operations
    from building up.


    B. Bang Pattern `!` (Syntactic Sugar for `seq`)

    This is just a cleaner way to write `seq`.

    ```Haskell
    go !acc = ...
    ```

    EFFECT: Identical to `seq`. It's preferred in modern Haskell because it's 
    cleaner.



    C. `deepseq` (The "Nuclear Option")

    `seq` only checks the "surface" ("is that a list node?"). `deepseq` checks
    the "contents" (is the integer inside fully calculated?).

    ```Haskell
    import Control.DeepSeq

    go acc = do
      x <- p
      let newAcc x : acc
      -- "Force the WHOLE TREE to be evaluated, then recurse"
      newAcc `deepSeq` go newAcc
    ```

    EFFECT: This forces `x` to be fully parsed and computed right now.

    WHEN TO USE: If `x` is a complex data structure (like a 1MB parsed JSON 
    object) and you want to ensure it's fully built to find errors now rather
    than later, or to free up memory references. For simple things like `Char`
    or `Int`, `deepseq` is overkill/identical to `seq`.



    ---

    3. VISUALIZING THE DIFFERENCE

    This is the key to understanding why the Accumulator + Strictness prevents
    the Boomerang.

                The Original (Boomerang / Stack Overflow)
                    many calls some, which calls many... Imagine parsing "ABC".

                    Stack Depth 0: cons 'A' (...) <--- WAITING

                    Stack Depth 1: cons 'B' (...) <--- WAITING

                    Stack Depth 2: cons 'C' (...) <--- WAITING

                    Stack Depth 3: pure [] (Found end)

                    Unwind: Now all those cons happen at once.

    MEMORY USAGE: Grows with input size (`O(N)`). 
    DANGER: If input is 1000000 chars, you crash.


    THE STRICT ACCUMULATOR (ITERATIVE / CONSTANT STACK)

    `go` calls `go` directly. Because of tail-call optimization, Haskell (GHC) turns this into a loop. It reuses the same stack frame!Loop 1: Parsed 'A'. Acc is ['A']. Jump to top.Loop 2: Parsed 'B'. Acc is ['B', 'A']. Jump to top.Loop 3: Parsed 'C'. Acc is ['C', 'B', 'A']. Jump to top.Loop 4: Failed. Return reverse acc (['A', 'B', 'C']).Memory usage: Constant stack usage ($O(1)$ stack frames). The heap grows to store the list, but the execution stack stays flat.Result: You can parse a 10GB file without crashing.SummaryThe Operators: seq and ! ensure the accumulator is calculated step-by-step rather than building a giant "to-do list" (thunk).The Pattern: You must switch to Accumulator style (tail recursion) to use them effectively.The Visual:Original: A diver swimming down, leaving a rope connected to the surface. He has to swim all the way back up to finish.Accumulator: A snowball rolling down a hill. It gets bigger as it goes, and when it hits the bottom, it's already done.

-}








---------------

{-
    ORIGINAL (THE BOOMERANGE)

    ```Haskell
    some :: Parser a -> Parser [a]
    some v = (:) <$> v <*> many v
    ```


    STRICT (THE SNOWBALL)

    ```Haskell
    some' :: Parser a -> Parser [a]
    some' p = 
      where
        go !acc = 
          do (
            x <- p
            go (x:acc)
          ) <|> pure (reverse acc)


          ===

        go !acc = (p >>= \x -> go (x:acc)) <|> pure (reverse acc)               <-- i guess this monadic notation make sense... becuase `go (x:acc)` is what that's turning into final value?
    ```
-}





{-
                        dont you need to strip p? like in manyStrict :: Parser a -> Parser [a]
                        why and how can you just do x <- p instead of first unwrapping it to (Parser p)?? oh wait, is it because your



            ANSWER:

                Yes, the `do` notation is doing the "stripping" for you.

                You have intuitively grasped the two different "levels" of coding in Haskell:
                1. THE ENGINE ROOM (Low Level): When you define the `Monad` instance itself,
                   you have to manually unwrap `(Parser p)` to touch the raw function.
                2. THE DRIVER SEAT (High Level): When you use the Monad (like in `manyStrict`),
                   you use `do` notation. You treat `p` as a black box.



                1. HOW `do` STRIPS THE WRAPPER

                    When you write this:

                    ```Haskell
                    do (
                      x <- p
                      ...    
                    )
                    ```

                    The compiler rewrites (desugars) it into this:

                    ```Haskell
                    p >>= (\x -> ...)
                    ```

                    The `>>=` operator calls `runParser` for you.
                    - It takes your opaque box `p`.
                    - It calls `runParser p` (the stripper).
                    - It runs the raw function on the state `s`.
                    - It takes the result `x` and hands it to your next line of code.


                    /// NOT COPYING ANYTHING BEYOND THIS... I THINK GEMINI IS GOING IN CIRCLES...
-}
                
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


{-
        HOW TO USE BANG PATTERNS

            To use bang patterns, add a pragma at the top of your Haskell file:

            ```Haskell
            {-# LANGUAGE BangPatterns #-}
            ```

            Note: If you are using GHC 2021 or later, this extension is enabled
            by default and the pragma is not necessaty.



        PURPOSE AND USAGE

            Haskell is a lazy language by default, meaning expressions are only
            evaluated when their results are needed. The `!` (bang) pattern 
            forces a value to be evaluated to weak head normal form (WHNF) 
            earlier than usual.

            This is primarily used for performance optimization and avoiding
            space leaks, particularly in numeric code or when processing large
            data structures.





        ---

        In Haskell, a Normal Form (NF) refers to a fully evaluated expression 
        with no unevaluated computations (thunks) remaining, essentially a 
        concrete value; this contrats with WEAK HEAD NORMAL FORM (WHNF), which
        is an expression reduced to its outermost constructor 
        (like `Just (..), (:) .., \x -> ...`) or a non-fully applied function,
        but its arguments might still be thunks.

        Haskell's laziness means expressions often stay in WHNF, but techniques
        like `deepseq` package force full evaluation to Normal Form, useful for
        performance or ensuring complete results.



        ---


        Once `instance Monad Parser where...` is written... 


        4. HOW THIS ENABLES `do` notation

        Once you write the code above, the compiler automatically allows you to
        write this:

        ```Haskell
        -- This `do` block
        myParser = do
          x <- char 'a'
          char 'b'
          return x

        -- ... is automatically rewritten by the compiler into this:
        myParser =
          char 'a' >>= \x ->
          char 'b' >>= \_ -> 
          return x
        ```

        You have now successfully build the "Engine Room". You can stop worrying
        about `runParser` and unwrapping, and start writing using `do` blocks!



-}




{-
        -- interesting. so for >>= for all monad instances. other than having m as a functor... which i mean ((->) r) or anything with
        -- type * -> *... then there really isn't anyuthing more complex it seems? because the case of just passing in a lsit looks very 
        -- simple to me
        --
        -- and ah... interestingly... for all monad types...inside >>=... it seems that the final type does concat regardless? or some 
        -- way to step down from m b inside (a -> m b)... as the final return type is just m b instead of m m b... ah but ig if we do a 
        -- different recursive structure... we dont need to do this, as this concat thing applies more towards a definition type where we 
        -- use list comprehension instead?
    
    
    You have just stumbled upon one of the deepest insights in Haskell (and 
    Category Theory).

    > "as the final return type is just `m b` instead of `m m b`"

    YES! THIS IS THE DEFINING CHARACERISTIC OF A MONAD.

    You asked if "concat"

-}

--------        ---------       --  ---------   --- -   -   -- --- - - -- ---
--------        ---------       --  ---------   --- -   -   -- --- - - -- ---




{-
    0. You have just stumbled upon one of the deepest insights in Haskell (and Category Theory).
    1. You are absolutely right. I should have led with the raw >>= definition and the out variable from the start. That is the mechanical heart of the whole system.
    2. 


    ---

    1. STATE-MONAD + Claude Monad
                The State Monad is the logical evolution of the Reader Monad.

    2. Check usage of Trees (for State monad and applicative-parsing techniques... look out for what are common 
       traits that's implemented... for better understanding of how things work in general...)
                3. Tree labelling / numbering   [CLAUDE]

    3. PPT L-System // PPT Calculus // PPT Turtle // PPT Tic-Tac-Toe <-- also look at tricks on preventing stack overflow by PPT-2... ask about similar techniques one can use for monads/applicatives/functors/alternatives/traversables

    5. 2025-2021 ... THEN unassessed exercises... THEN 2009-2013

    
-}