{-
             .-.'  '.-.
          .-(   \  /   )-.
         /   '..oOOo..'   \
 ,       \.--.oOOOOOOo.--./
 |\  ,   (   :oOOOOOOo:   )
_\.\/|   /'--'oOOOOOOo'--'\
'-.. ;/| \   .''oOOo''.   /
.--`'. :/|'-(   /  \   )-'
 '--. `. / //'-'.__.'-;
   `'-,_';//      ,  /|
        '((       |\/./_
          \\  . |\; ..-'
           \\ |\: .'`--.
            \\, .' .--'
             ))'_,-'`
            //-'
           // 
          //
         |/
-}

{-
    The concept you are describing--that functions are just "things" that can be
    thrown around, stored in lists, or passed as arguments even if they aren't 
    "finished" yet--is called treating functions as First-Class citizens.

    > throwing around incomplete functions --> The key to everything in Haskell


    1. Functions are just "Blueprints"

    In many languages (like C or old Java), a function is an Action. You call it
    , it runs, it finishes. You can't really hold "the concept of adding" in 
    your hand; you can only do the adding.

    In Haskell, a function is a VALUE, just like an Integer or a String.
    - `5` is a value representing a number.
    - `"Hello"` is a value representing text.
    - `(+1)` is a value representing a TRANSFORMATION BLUEPRINT.

    When you write `fmap (+1)`, you aren't running code. You are handling the 
    `fmap` machine a blueprint and saying: "Here, keep this blueprint in your
    pocket. Use it later when the data arrives."


    2. The Power of "Incomplete" (Partial Application)

    You mentioned functions that are "larger or smaller and that require more or
    less input arguments."

    This is the superpower of CURRYING.

    Imagine a function `complexCalc` that needs 3 inputs: 
    `Config -> User -> Data -> Result`.

    In Haskell, you don't need all 3 at once!
    1. Step 1: You pass in the `Config`.
        - Result: A generic "User Processor" machine (waitin for User and Data).
        - You can pass this machine to the "User Login Module."
    2. Step 2: The Login Module adds the `User`.
        - Result: A specific "Data Handler" machine (waiting for Data).
        - You can pass this machine to the "Database Module."
    3. Step 3: The Database Module finds the `Data`.
        - Action: It finally runs!

    You effectively "threw around" the function through three different parts of
    your codebase, and each added a little bit more context to it.


    3. The "Factory Belt" Analogy

    Since you liked the Vending Machine, here is the visual for COMPOSITION and
    HIGHER ORDER FUNCTIONS.

    Imagine a factory conveyor belt.
    - The Data is the product on the belt.
    - The Functions are the robot arms bolted onto the line.


    When you write code like `f . g. h`, you are not running the factory. You 
    are designing the assembly line.
    - You buy a robot arm `h`.
    - You buy a robot arm `g`.
    - You bolt them together.
    - You put the whole assembly in a truck and send it to another factory 
      (`fmap`).

    The entire time, there is no product (no `x`). You are just manipulating the 
    machinery itself.

    
    Summary
    - Functions are Objects: You can put them in lists `[(+1), (*2)]`. You can
      return them. You can pass them.
    - Partial Application: You can fill a function halfway and pass the 
      "half-filled" version to someone else to finish.
    - The "Invisible X": The reason you don't see the input variables is that
      you are acting as the ARCHITECT of the factory, not the OPERATOR. You are
      wiring the machines; you aren't holding the raw materials.
-}

---     --  -----       --- -   ------          -       --- --- -- - -----  ----
---     --  -----       --- -   ------          -       --- --- -- - -----  ----


{-
    The Functor Laws

    Every Functor instance must obey two laws:


    Law 1: Identiy
    ```Haskell
    fmap id = id
    ```

    If you map the identity function, nothing changes. This guarantees `fmap` 
    can't secretly alter the structure.


    Law 2: Composition
    ```Haskell
    fmap f . fmap g = fmap (f . g)
    ```

    Mapping twice is the same as mapping once with the composed function. This
    is also an optimization--two passed can become one.

    Why do laws matter for exams? You might be asked to:
    1. Prove an instance satisfies the laws
    2. Identify a broken instance that violates them
-}
---     --  -----       --- -   ------          -       --- --- -- - -----  ----

-- The Mechanical Recipe for Writing Instances

{-
    When writing a Functor instance, follow this recipe:
    1. For each construcot, look at where type `a` appears
    2. Apply `f` to every `a` you find
    3. For recursive positions (like `Tree a` inside `Tree`), use `fmap f`
    4. For nested functors (like `[Tree a]`), use `fmap (fmap f)`


    Example with RoseBush:
-}

data RoseBush a = Flower a | Vine [RoseBush a]

instance Functor RoseBush where
    fmap :: (a -> b) -> RoseBush a -> RoseBush b
    fmap f (Flower x)    = Flower (f x)
    fmap f (Vine xs) = Vine (fmap f x) : Vine (fmap (fmap f) xs)
    -- ts :: [RoseBush a] 
    -- outer fmap: maps over the list
    -- inner fmap: maps over each RoseBush

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

    Exam Insights and trick


    Common Exam Questions:
        1. "Write the Functor instance for this datatype"
          - Follow the mechanical recipe above
            1. For each construcotr, look at where type `a` appears
            2. Apply `f` to every `a` you find
            3. For recursive positions (like `Tree a` inside `Tree`), use 
               `fmap f`
            4. For nested functors (like `[Tree a]`), use `fmap (fmap f)`
          - Remember: only transfer the last type parameter
          - Use `fmap` recursively for nested structures
        
        2. "What is the kind of X?"
          - Count how many type parameters it takes
          - `Bool :: *` (no parameters)
          - `Maybe :: * -> *` (one paramter)
          - `Either :: * -> * -> *` (two paramters)
          - `Either Int :: * -> *` (partially applied)

        3. "Prove the Functor laws for this instance"
          - Do case analysis on each constructor
          - Show `fmap id x = x` for each case
          - Show `fmap f (fmap g x) = fmap (f . g) x` for each case
        
        4. "Why can't X be a Functor?"
          - Usually because `a` appears in a "Negative position" (as input to
            a function)
          - `data Pred a = Pred (a -> Bool)` can't be a Functor--we'd need to
            turn `a`s into `b`s, but `a` is consumed, not produced...


        Quick Tricks:
          - If you see `fmap f . fmap g` in code, you can always rewrite as
            `fmap (f . g)` for efficiency
          - `fmap f Nothing = Nothing` always--there's nothing to map over
          - `fmap f (Left x) = Left x` always--Left values are "errors" that 
            pass through unchanged
          - When in doubt about kinds, ask: "Can I have a value of this type?"
            If yes, it's `*`. If it needs more type arguments, add `-> *` for
            each one.


                The Limitation of Functor
                Functor lets us transform values inside a structure, but it can't combine structures. If you have:
                haskellmx :: Maybe Int
                my :: Maybe Int
                And you want to add them, fmap alone can't help—it works on one structure at a time. You'd need:
                haskelladdMaybe :: Maybe Int -> Maybe Int -> Maybe Int
                addMaybe (Just x) (Just y) = Just (x + y)
                addMaybe _ _ = Nothing
                But this is specific to Maybe. Tomorrow's topic (Applicative) generalizes this pattern...


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
---     --  -----       --- -   ------          -       --- --- -- - -----  ----


{-
    Let's apply `add 5` to five `Maybe Int` values:

    ```Haskell
    add5 <$> Just 1 <*> Just 2 <*> Just 3 <*> Just 4 <*> Just 5
    ```

    Step 1: `add5 <*> Just 1`

    ```Haskell
    (<$>) :: (a -> b) -> f a -> f b           -- <-- alias for fmap

    add 5   :: Int -> (Int -> Int -> Int -> Int -> Int)
    Just 1  :: Maybe Int

    -- Here: a = Int, b = (Int -> Int -> Int -> Int -> Int)
    -- So (<$>) applies add5 to the 1 inside the Just

    add5 <$> Just 1
        = Just (add5 1)
        = Just (\b c d e -> 1 + b + c + d + e)
        :: Maybe (Int -> Int -> Int -> Int -> Int)

    ```

    This is where the container comes from! `<$>` (which is `fmap`) takes a 
    plain function and a wrapped value, applies the function inside the wrapper
    , and gives you back a wrapped result. The function is now inside the 
    `Maybe`.

    ---

    
    Step 2: `... <*> Just 2`

    Now we have a function inside a Maybe, and we want to apply it another
    Maybe value:

    ---

    `(<*>) :: Applicative f => f (a -> b) -> f a -> f b`
            <-- ah i think i can kind of see it now, (<*>) is basically the same
                as (<$>), with the only difference being that (<$>) outputs
                it's curried return function inside container Applicative f...
                
                Hence, (<*>) needs to accept the function input as `f (a -> b)`
    ...

    
    ---

    
    What Happens with Nothing?

    ```Haskell
    add5 <$> Just 1 <*> Nothing <*> Just 3 <*> Just 4 <*> Just 5
    ```

    As soon as you hit `Nothing`:

    ```Haskell
    Just (\c d e -> 3 + c + d + e) <*> Nothing
        = Nothing  -- can't apply a function to "no value"
        :: Maybe (Int -> Int -> Int)
    ```

    And then:

    ```Haskell
    Nothing <*> Just 4 = Nothing
    Nothing <*> Just 5 = Nothing
    ```

    The `Nothing` propagates through. This is why Applicative is useful--it 
    handles the "what if something fails" logic automatically.

                ghci> :type Nothing
                Nothing :: Maybe a
                ghci> :type Nothing :: Maybe (Int -> Int)
                Nothing :: Maybe (Int -> Int) :: Maybe (Int -> Int)

-}