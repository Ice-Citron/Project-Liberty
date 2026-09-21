{-

    - Functor is a typeclass (like `Eq` or `Ord`)
    - Maybe is a type construcotr that has an instance of the Functor typeclass
    - We often casually say "Maybe is a Functor" meaning "Maybe has a Functor
      instance"

    It's like saying "Int is Eq"--we mean Int has an instance of the Eq 
    typeclass.

    And yes, `Maybe` is an instance of many typeclasses:
    ```Haskell
        ghci> :info Maybe
        instance Functor Maybe
        instance Applicative Maybe
        instance Foldable Maybe
        instance Monad Maybe
        -- etc.
    ```

    The "container" intuition is a useful mental model, but technically Functor
    is the typeclass, and types like `Maybe`, `[]`, `Either e`, `Tree`, `IO` are
    all instances of it.


    ---

    Exam Insight

    If you're asked to trace through an applicative chain, the key is tracking 
    the type at each step. The `b` in `f (a -> b)` keeps shrinking as you apply 
    more arguments, until eventually `b` is not a function anymore and you're 
    done.


-}

---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----



{-
         ,
     /\^/`\
    | \/   |
    | |    |
    \ \    /
     '\\//'
       ||
       ||
       ||
       ||  ,
   |\  ||  |\
   | | ||  | |
   | | || / /
    \ \||/ /
     `\\//`
    ^^^^^^^^
-}

{-
    FUNCTOR is about applying a pure (normal) function to a value that is stuck
    inside a container (context).

    APPLICATIVE solves a specific problem that Functors simply cannot handle:
    What if the funciton itself is stuck inside a container?


    1. The Scenario: The Limit of Functor

    Imagine you want to add two numbers, but both are inside `Maybe` boxes.

    ```Haskell
    val1 = Just 5
    val2 = Just 3
    add :: Int -> Int -> Int
    add x y = x + y
    ```

            Attempt 1: Using Functor (fmap) Let's try to apply add to the first value using fmap:

            Haskell

            -- fmap :: (a -> b) -> f a -> f b
            step1 = fmap add val1
            What is the result of step1?

            add takes 2 arguments. We gave it 1 (the 5 inside the box).

            The result is a partial function (+5)... but it is stuck inside the box!

            step1 :: Maybe (Int -> Int)

            Value: Just (+5)

            Attempt 2: The Blockage Now we want to apply this Just (+5) to val2 (Just 3). Can we use fmap again?

            Haskell

            -- fmap :: (a -> b) -> f a -> f b
            step2 = fmap step1 val2


    ERROR!
    - `fmap` expects a pure function `(a -> b)` as its first argument.
    - We gave it `step1`, which is a wrapped function `Maybe (Int -> Int)`
                                                           -- same as f (a -> b)
    - `fmap` doesn't know how to handle a function that's already inside a box.

    
    ---

    2. THE SOLUTION: Applicative (`<*>`)

    This is exactly what Applicative is for. It defines the operator `<*>` 
    (pronounced "apply"). 

    Its job is to take a wrapped function and apply it to a wrapped value.


    The Signature Comprison
    - Functor       `fmap` or `(<$>)`
        * Signature: `(a -> b) -> f a -> f b`
        * Meaning: Apply PURE function to wrapped value
    - Applicative   `(<*>)`
        * Signature: `f (a -> b) -> f a -> f b`
        * Meaning: Apply WRAPPED function to wrapped value


    ```Haskell
    add <$> val1 <*> val2

    <=>

    (fmap add val1) <*> val2
    ```


    ---

    3. The "Applicative Style"


                3. The "Applicative Style"
                Haskell developers rarely write it in two steps. They chain it together in a pattern that looks almost like a normal function call.

                Normal Function Call:

                Haskell

                add 5 3
                Applicative Function Call: We use <$> (which is just fmap) and <*> to mimic the spacing of the normal call.

                Haskell

                add <$> Just 5 <*> Just 3
                add <$> Just 5 runs first.

                This is fmap. It puts add into the context.

                Result: Just (+5)

                ... <*> Just 3 runs next.

                This takes the Just (+5) and applies it to Just 3.

                Result: Just 8


    Summary
    - FUNCTOR: You have a clean tool (function) and a dirty object (value in 
        context). You reach in with gloves (`fmap`) to use the tool.
    - APPLICATIVE: You have a dirty tool (function in context) and a dirty 
        object. You need a way (`<*>`) to smash the contexts together so the
        tool can touch the object.

    

    ---

    Analogy:
    - FUNCTOR (`* -> *`): A Cookie Cutter. (You can put dough in it to make a 
      cookie).
    - CONCRETE TYPE (`*`): A Baked Cookie. (You can't put dough in it; it's
      already done).
-}



---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----

---     --  -----       --- -   ------          -       --- --- -- - -----  ----


{-
         *
     *  *r*  *
  * *a* ^Y^ *i* *
 *m*^Y^*^\^*^Y^*s*
 ^Y^*\*e*/*l*/*^Y^
 *\*t*|Y^\^Y|*l*/*
*s*|Y^\\^/^//^Y|*a*
^Y^\\_^\\\//^_//^Y^
^\_^\_\_\//_/_/^_/^
 ^^\_^\_\\/_/^_/^^
   ^^\_ \// _/^^
       \_\_/
        /|\
       /\\/\
-}



---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----


{-
    This is final boss of the "Functions as Containers" arc. If you understand 
    this, you understand the architecture used in massive Haskell applications.

    The `Monad` instance for functions `((->) r)` is famously called the Reader
    Monad.

    Its superpower is IMPLICIT CONFIGURATION. It lets you access a "global 
    variable" (like a config or environment) anywhere in your code without 
    passing it around manually as an argument.

    ---


    1. THE GOAL: WHY DO YOU NEED A MONAD?

    Functor (`fmap`) lets you change the OUTPUT of a function. Applicative 
    (`<*>`) lets you combine INDEPENDENT functions that share an input.

    MONAD (`>>=`) lets you make decisions based on the previous result, whilst
    STILL SHARING THE INPUT.
    - SCENARIO: You have a function `getValidUser`. It needs the `Config` to 
      run.
    - NEXT STEP: Based on which user you found, you want to run `getRecentPosts`
      . This also needs the `Config` to connect to the DB.
    - PROBLEM: You need to pass the `Config` to the first function, get the 
      result, and then pass the same `Config` (and the result) to the second 
      function.

    ---


    2. DERIVING THE TYPE SIGNATURE

    The Monad definition is:
    ```Haskell
    (>>=) :: m a -> (a -> m b) -> m b
    ```

    Let's replace `m` with `((->) r)` (The Function context):
    1. `m a` becomes `(r -> a)`
       - Meaning: A computation that needs `r` to produce an `a`.
    2. `a -> m b` becomes `a -> (r -> b)`
       - Meaning: A function that takes the result `a`, and returns a new 
         computation that needs `r`.
    3. `m b` becomes `(r -> b)`
       - Meaning: The final result is a computation waiting for `r`.

    
    THE FINAL SIGNATURE:
    ```Haskell
    (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
    ```

    ---


    3. IMPLEMENTING THE LOGIC (THE "SPLITTER")

    We need to write a function that takes the environment `r` (the 
    "dependency") and hands it to EVERYONE.

    ```Haskell
    instance Monad ((->) r) where
        -- h is the first computation (r -> a)
        -- f is the next step (a -> r -> b)
        h >>= f = \r -> ...
        (>>=) h f = (\r -> ...)
    ```

    We are returning a function waiting for `r`. What do we do with `r`?
    1. STEP 1: Run the first computation `h` using `r`. `let a = h r`
    2. STEP 2: Give the result `a` to the function `f`. `let nextFunction = f a`
       (Note: `nextFunction` is now type `r -> b`)
    3. STEP 3: Run that next function! But it also needs `r`. So we give it the
       SAME `r`. `nextFunction r`

    
    THE ONE-LINER:
    ```Haskell
    h >>= f = (\r -> f (h r) r)
    ```

    WHAT JUST HAPPENED? We created a "Y-splitter". The single input `r` was 
    duplicated and fed to both `h` (the producer) and `f` (the consumer).


    ---

    4. WHY IS THIS "DEPENDENCY INJECTION"?

    Let's look at code WITHOUT the Monad (The Painful Way), and then WITH the 
    Monad (The Magic Way).

    THE SETUP: We have a `Config` object with a secret API key.

    ```Haskell
    data Config = Config { apiKey :: String }

    -- Function 1: Needs Config
    authenticate :: Config -> Bool
    authenticate config = (apikey config) == "12345"

    -- Function 2: Needs Config AND the result of auth
    getData :: Bool -> Config -> String
    getData isAuth config = 
      if isAuth && (apiKey config == "12345")
      then "Secret Data"
      else "Access Denied"
    ```


    THE "PAINFUL" WAY (Manual Parsing)

    You have to manually thread the `cfg` variable everywhere.

    ```Haskell
    mainRun :: Config -> String
    mainRun cfg =
      let isAuth = authenticate cfg     -- Pass cfg manually
      in  getData isAuth cfg            -- Pass cfg manuall AGAIN
    ```


    THE "MONADIC" WAY (Dependency Injection)

    Using `>>=` (or `do` notation), the `cfg` disappears from the code!

    ```Haskell
    mainRun :: Config -> String
    mainRun cfg = (authenticate >>= getData) cfg
    ```


    WAIT, HOW?
    1. `authenticate` is actually a function waiting for config.
    2. The `Monad` instance (our `h >>= f` implementation)
    3. It takes the result (`isAuth`) and passe it top `getData`.
    4. Crucially, it ALSO silently passes that same config to `getData`.


    SUMMARY
    - The Container: The "Function Monad" is a computation waiting for an 
      environment (`r`).
    - The Bind (`>>=`): It creates a pipeline where EVERY STEP receives a copy
      of the same environment automatically.
    - The Benefit: You write code that looks like it has global access to `r`, 
      but it's actually purely functional "Dependency Injection".

    -- exact mechnic behind the READER MONAD... the industry standard for 
    -- handling configurations in Haskell apps...
    


    ---
-}

---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----


{-

    For functions, the Monad instance is (Reader):

    ```Haskell
    (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
    f >>= k = (\r -> k (f r) r)
    ```


    `do`-block desugars to:
    ```Haskell
    mainRun = authenticate >>= \isAuth -> getData isAuth
    ```

    But since `getData :: Bool -> Config -> String` already matches 
    `\isAuth -> getData isAuth`, you can eta-reduce:

    ```Haskell
    mainRun = authenticate >>= getData
    ```



    ALL EQUIVALENT FORMS
    ```Haskell
    mainRun :: Config -> Strng
    mainRun cfg = (authenticate >>= getData) cfg

    mainRun :: Config -> String
    mainRun = (authenticate >>= getData)            -- eta-reduced form

    mainRun :: Config -> String
    mainRun cfg = getData (authenticate cfg) cfg

    mainRun :: Config -> String
    mainRun = do
      isAuth <- authenticate
      getData isAuth
    ```                 --  getData (authenticate cfg) cfg
-}
---     --  -----       --- -   ------          -       --- --- -- - -----  ----



{-
    ONE CAVEAT

        Eta-reduction can change behavior in rare cases involving `seq`/bottom
        on function values, but in normal monadic code it's usually safe and 
        used as a style convenience--not a monad-specific feature or automatic
        "always reduce" rule.


    ---
                Lambda-functions ARE values in Haskell
    ---


                In “pure” math / lambda calculus, eta-reduction is always safe. In Haskell, it can change behavior because Haskell has:

                bottom (⊥): values that crash or don’t terminate (undefined, error, infinite loop)

                seq (and ! / deepseq): operations that can observe whether something is already evaluated to WHNF

                Key fact: a lambda is already in WHNF

                A function value like \x -> ... is in WHNF immediately. Its body is not evaluated until you apply it.

                seq forces only to WHNF:

                seq :: a -> b -> b
                -- Conceptually: seq a b = case a of _ -> b

                The classic counterexample
                f :: Int -> Int
                f = undefined

                g :: Int -> Int
                g x = f x     -- eta-expanded wrapper


                Now compare:

                f `seq` "ok"   -- crashes (because f is ⊥, forcing it to WHNF diverges)
                g `seq` "ok"   -- "ok"   (because g is a lambda, already WHNF)


                But extensionally (as functions), they behave the same when applied:

                f 1  -- crashes
                g 1  -- crashes


                So eta-reducing g x = f x into g = f changes whether g is bottom as a value, which seq can detect.

                This also shows up with strictness annotations

                A strict field (or bang pattern) uses seq-like forcing to WHNF. So:

                data Wrap a = Wrap !a


                Then:

                Wrap f crashes (forces f to WHNF, but f is ⊥)

                Wrap g is fine (forces g to WHNF, and g is a lambda)

                Again: eta-reduction can change behavior if you ever force a function value.

                When you can ignore this

                In most normal code, you don’t seq function values or store them in strict fields, so eta-reduction is effectively “safe” and just a readability/style choice.

                The warning matters mainly in performance/strictness-heavy code, or when you deliberately use seq/deepseq/bang patterns around function values.

                im confused. cuz i thought that seq and ! or bang was used mainly because you want to prevent thunking??

                or maybe seq :: a -> b -> b works becuase it forces a to be evaluated first? sorry, im confued. but by just passing a into the seq function which seq never uses a... even if seq is evaluated at last... wont the thunking problem perhaps still exist? or maybe this is circumvented because seq is special, and hence uhhh. it forces a to be evaluated right away??? sorry, im just confused. thanks
                Thought for 25s

                seq/! are exactly about controlling thunking/space usage — but the crucial detail is what “forcing” means in a lazy language, and why seq “uses” its first argument even though it returns the second.

                1) Laziness and thunking (what’s the problem?)

                In Haskell, an expression is not evaluated until something needs its value. Until then it’s stored as a thunk (a suspended computation).

                Thunk buildup happens when you keep constructing bigger and bigger unevaluated expressions. Classic example:

                sumBad :: Int -> [Int] -> Int
                sumBad acc []     = acc
                sumBad acc (x:xs) = sumBad (acc + x) xs


                Because acc + x isn’t forced, acc can become a huge thunk like:

                (((0 + x1) + x2) + x3) + ...


                This can blow memory.

                2) What seq actually guarantees

                Type:

                seq :: a -> b -> b


                Semantics (the right mental model):

                seq a b = case a of
                            _ -> b


                That case is the key: pattern matching forces evaluation to WHNF.

                So:

                seq is strict in its first argument: it forces a to WHNF

                seq is lazy in its second argument: it returns b without forcing b (unless needed later)

                Meaning:

                a seq b guarantees that if you ever get b, then a has been evaluated to WHNF first.

                So even though seq “doesn’t return a”, it does “use” a to check whether it’s bottom and to force it to WHNF.

                seq is not an ordinary function you could implement yourself purely from that polymorphic type; it’s a primitive with special evaluation rules.

                3) “Does it force right away?”

                Not “right away” in wall-clock time; “right away” in evaluation order.

                If the program needs the result of a seq b, it must evaluate a (to WHNF) before it can produce b.

                If nobody ever demands that result, nothing happens (still lazy overall).

                Example:

                xa = x `seq` ()


                If xa is demanded, x is forced to WHNF first, then () is returned.

                4) Why this helps with thunking

                You insert seq at points where you want to prevent building a giant delayed expression.

                The strict version of the accumulator example:

                sumGood :: Int -> [Int] -> Int
                sumGood acc []     = acc
                sumGood acc (x:xs) =
                let acc' = acc + x
                in acc' `seq` sumGood acc' xs


                Now each acc' is forced to WHNF at every step, so you don’t accumulate a massive thunk chain.

                This is exactly what foldl' does (conceptually).

                5) What ! (bang patterns) are

                A bang pattern is basically syntactic sugar for seq at a binding/pattern match:

                let !x = e in body


                means: evaluate e to WHNF before continuing with body.

                Similarly, strict fields (data T = T !a) force the field to WHNF when you construct the value.

                6) WHNF vs “fully evaluated”

                Important: seq/! force only to WHNF, not full normal form.

                For an Int, WHNF is the actual number (so it’s “fully computed”).

                For a list, WHNF is only whether it’s [] or (:) ... (i.e., the first cons cell). The tail can still be a thunk.

                To force the entire structure, you use deepseq (needs NFData).

                So in your earlier list example:

                x = [1..1000000]
                x `seq` ()


                This forces enough to know x is a cons (1 : ...), but it does not build the whole list.

                7) How this connects to the eta-reduction “rare behavior change”

                A function value like \x -> ... is already in WHNF (it’s a lambda). So:

                (\x -> ...) seq something succeeds immediately

                undefined seq something crashes

                Eta-reduction can change whether you have a lambda or a bottom as a value, which seq can observe.
-}