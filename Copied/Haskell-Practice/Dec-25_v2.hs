-- a Functor is a TYPECLASS
-- a Typeclass, you can think of it as an interface

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b

-- data Maybe a = Just a | Nothing

{-
        ghci> (+3) <$> Just 5
        Just 8

        ghci> fmap (+3) (Just 6)
        Just 9
-}


data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2  = Nothing2
{-
        ghci> fmap (+3) (Just2 56)
        Just2 59
        ghci> (+3) <$> Just2 56
        Just2 59

        ghci> fmap (fmap (+3)) [Just2 2, Just2 67]
        [Just2 5,Just2 70]
        ghci> ((+3) <$>) <$> [Just2 3, Just2 121]
        [Just2 6,Just2 124]
-}

-- Haskell defines Functor for Maybe, Either, Lists

data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Tip a)        = Tip (f a)
    fmap f (Branch lt rt) = Branch (fmap f lt) (fmap f rt)  -- fmap f rt <=> f <$> rt



------ ------       ------ ------                ------ ------ ------ ------

{-
    Applicative is also a typeclass, and a typeclass is basically an interface

    An interface is where we have the signature for some methods and we 
    essentially have to develop those methods or basically
-}

-- This is how an applicative is defined
{-
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

add5 :: Int -> Int -> Int -> Int -> Int -> Int
add5 a b c d e = a + b + c + d + e

      -- due to currying, both are equivalent. so technically. both are (a -> b)
add5' :: Int -> (Int -> (Int -> (Int -> (Int -> Int))))
add5' = undefined

{-
        add5 <$> Just 1 <*> Just 2 <*> Just 3 <*> Just 4 <*> Just 5


        add5 <$> Just 1 
        = Just (add5 1)
        = Just (\b c d e -> 1 + b + c + d + e)
        :: Maybe (Int -> Int -> Int -> Int -> Int)


        ghci> :type (<*>)
        (<*>) :: Applicative f => f (a -> b) -> f a -> f b
        ghci> :type (<$>)
        (<$>) :: Functor f => (a -> b) -> f a -> f b
-}



{-
data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2  = Nothing2
-}

instance Applicative Maybe2 where
    pure :: a -> Maybe2 a
    pure = Just2          -- Just2 :: a -> Maybe2 a
            -- eta-reduction for `pure x = Just2 x`

    (<*>) :: Maybe2 (a -> b) -> Maybe2 a -> Maybe2 b
    (Just2 func) <*> (Just2 x) = Just2 (func x)
    (Just2 func) <*> j         = fmap func j -- fmap (+3) (Just2 1) // duplicate of above
    Nothing2     <*> j         = Nothing2

{-
            ghci> pure 3 :: Maybe Int
            Just 3

    Intuition: `pure` should inject a value with "no effects". For `Maybe`-like
    types, the "no failure" case is the `Just` constructor.

    With your `(<*>)` (which is essentially the standard `Maybe` applicative),
    the Applicative laws work out, e.g. identity:

            pure id <*> v
            = Just2 id <*> v
            = fmap id v
            = v


    ---

    In programming, eta-reduction (or η-reduction) is a simplifcation rule in 
    lambda calculus that removes an unnecessary function argument, transforming
    `\x. f(x)` into just `f`, as long as `x` isn't used elsewhere in f. It makes
    code more concise, readable and sometimes more efficient by removing 
    redundant function wrappers, a core technique for point-free style in 
    functional programming.            
-}

----- -----  ----- -----        ----- -----       ----- ----- 



{-
data Tree a = Tip a | Branch (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Tip a)        = Tip (f a)
    fmap f (Branch lt rt) = Branch (fmap f lt) (fmap f rt)  -- fmap f rt <=> f <$> rt
-}

instance Applicative Tree where
    pure :: a -> Tree a
    pure = Tip

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Branch fl fr) (Tip a)        = Tip (func a)
    (<*>) (Branch fl fr) (Branch lt rt) = Branch (Tree func <*> lt) (Tree func <*> rt)
    (<*>) (Tip f)     _              = fmap f t

    {-
        Type `Tree (a -> b)` means the left argument is not "a function", but
        rather a TREE OF FUNCTIONS.

        That means a value like

        ```
        Branch fl fr :: Tree (a -> b)
        ```

        literally contains potentially diferent functions in the left and right
        subtrees. So `<*>` has to say what it means to apply a function tree to
        a value tree. 
    -}




------ ------       ------ ------                ------ ------ ------ ------
------ ------       ------ ------                ------ ------ ------ ------
------ ------       ------ ------                ------ ------ ------ ------

{-
                You're asking exactly the right questions. Let me trace through this step by step, showing the types at every stage.

                ## The Setup: A 5-argument Function

                ```haskell
                add5 :: Int -> Int -> Int -> Int -> Int -> Int
                add5 a b c d e = a + b + c + d + e
                ```

                Remember currying: this type is actually:
                ```haskell
                add5 :: Int -> (Int -> (Int -> (Int -> (Int -> Int))))
                ```

                So `add5` takes one Int, and returns a function waiting for the next Int, and so on.

                ---

                ## Step-by-Step Trace with Types

                Let's apply `add5` to five `Maybe Int` values:

                ```haskell
                add5 <$> Just 1 <*> Just 2 <*> Just 3 <*> Just 4 <*> Just 5
                ```

                **Step 1: `add5 <$> Just 1`**

                ```haskell
                (<$>) :: (a -> b) -> f a -> f b

                add5     :: Int -> (Int -> Int -> Int -> Int -> Int)
                Just 1   :: Maybe Int

                -- Here: a = Int, b = (Int -> Int -> Int -> Int -> Int), f = Maybe
                -- So (<$>) applies add5 to the 1 inside the Just

                add5 <$> Just 1 
                = Just (add5 1)
                = Just (\b c d e -> 1 + b + c + d + e)
                :: Maybe (Int -> Int -> Int -> Int -> Int)
                ```

                **This is where the container comes from!** `<$>` (which is `fmap`) takes a plain function and a wrapped value, applies the function *inside* the wrapper, and gives you back a wrapped result. The function is now *inside* the `Maybe`.

                ---

                **Step 2: `... <*> Just 2`**

                Now we have a function *inside* a Maybe, and we want to apply it to another Maybe value:

                ```haskell
                (<*>) :: f (a -> b) -> f a -> f b

                Just (\b c d e -> 1 + b + c + d + e) :: Maybe (Int -> Int -> Int -> Int -> Int)
                Just 2                               :: Maybe Int

                -- Here: a = Int, b = (Int -> Int -> Int -> Int), f = Maybe
                -- <*> unwraps both, applies the function, rewraps

                Just (\b c d e -> 1 + b + c + d + e) <*> Just 2
                = Just ((\b c d e -> 1 + b + c + d + e) 2)
                = Just (\c d e -> 1 + 2 + c + d + e)
                :: Maybe (Int -> Int -> Int -> Int)
                ```

                ---

                **Step 3: `... <*> Just 3`**

                ```haskell
                Just (\c d e -> 1 + 2 + c + d + e) :: Maybe (Int -> Int -> Int -> Int)
                Just 3                             :: Maybe Int

                -- a = Int, b = (Int -> Int -> Int)

                Just (\c d e -> 3 + c + d + e) <*> Just 3
                = Just (\d e -> 3 + 3 + d + e)
                :: Maybe (Int -> Int -> Int)
                ```

                ---

                **Step 4: `... <*> Just 4`**

                ```haskell
                Just (\d e -> 6 + d + e) :: Maybe (Int -> Int -> Int)
                Just 4                   :: Maybe Int

                -- a = Int, b = (Int -> Int)

                ... <*> Just 4
                = Just (\e -> 6 + 4 + e)
                :: Maybe (Int -> Int)
                ```

                ---

                **Step 5: `... <*> Just 5`**

                ```haskell
                Just (\e -> 10 + e) :: Maybe (Int -> Int)
                Just 5              :: Maybe Int

                -- a = Int, b = Int

                ... <*> Just 5
                = Just (10 + 5)
                = Just 15
                :: Maybe Int
                ```

                ---

                ## The Pattern Visualized

                ```
                add5 <$> Just 1  <*> Just 2  <*> Just 3  <*> Just 4  <*> Just 5
                    │          │           │           │           │
                    ▼          ▼           ▼           ▼           ▼
                    
                Int -> Int -> Int -> Int -> Int -> Int     -- original function
                    │
                    ▼ apply to 1, wrap in Maybe
                Maybe (Int -> Int -> Int -> Int -> Int)    -- after <$>
                            │
                            ▼ apply to 2
                    Maybe (Int -> Int -> Int -> Int)     -- after first <*>
                                │
                                ▼ apply to 3
                            Maybe (Int -> Int -> Int)      -- after second <*>
                                        │
                                        ▼ apply to 4
                                Maybe (Int -> Int)       -- after third <*>
                                            │
                                            ▼ apply to 5
                                        Maybe Int          -- final result
                ```

                ---

                ## What Happens with Nothing?

                ```haskell
                add5 <$> Just 1 <*> Nothing <*> Just 3 <*> Just 4 <*> Just 5
                ```

                As soon as you hit `Nothing`:

                ```haskell
                Just (\c d e -> 3 + c + d + e) <*> Nothing
                = Nothing  -- can't apply a function to "no value"
                :: Maybe (Int -> Int -> Int)
                ```

                And then:
                ```haskell
                Nothing <*> Just 4 = Nothing
                Nothing <*> Just 5 = Nothing
                ```

                The `Nothing` propagates through. This is why Applicative is useful—it handles the "what if something fails" logic automatically.

                ---

                ## Your Terminology Question

                You're right that this can be confusing:

                - **Functor** is a *typeclass* (like `Eq` or `Ord`)
                - **Maybe** is a *type constructor* that has an *instance* of the Functor typeclass
                - We often casually say "Maybe is a Functor" meaning "Maybe has a Functor instance"

                It's like saying "Int is Eq"—we mean Int has an instance of the Eq typeclass.

                And yes, `Maybe` is an instance of many typeclasses:
                ```haskell
                ghci> :info Maybe
                instance Functor Maybe
                instance Applicative Maybe
                instance Foldable Maybe
                instance Monad Maybe
                -- etc.
                ```

                The "container" intuition is a useful mental model, but technically Functor is the typeclass, and types like `Maybe`, `[]`, `Either e`, `Tree`, `IO` are all instances of it.

                ---

                ## Exam Insight

                If you're asked to trace through an applicative chain, the key is tracking the type at each step. The `b` in `f (a -> b)` keeps shrinking as you apply more arguments, until eventually `b` is not a function anymore and you're done.

                Want me to show you the same thing with lists (cartesian product behavior) or with a custom datatype?

-}

------ ------       ------ ------                ------ ------ ------ ------