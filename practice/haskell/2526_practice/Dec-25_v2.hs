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
    pure = Tip          -- pure x = Tip x

    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (<*>) (Branch fl fr) (Tip x)        = Branch (fl <*> Tip x)  (fr <*> Tip x)
    (<*>) (Branch fl fr) (Branch lt rt) = Branch (fl <*> lt) (fr <*> rt)
    (<*>) (Tip f)        t              = fmap f t

        {-
            Then identity holds

            ```Haskell
            pure id <*> t
            = Tip id <*> t
            = fmap id t
            = t
            ```

            If instead you wanted a strict zip applicative that only works when 
            shapes match, then `pure = Tip` would usually not work (because
            `pure id <*> Branch ...` must equal `Branch ...`), and you'd need a
            different `pure` (often an infinite tree filled with the value).

            So:
            - `pure = Just2` is straightforwardly correct for your `Maybe2`.
            - `pure = Tip` is a reasonable/correct choice for `Tree`, but only
              with an appropriate `<*>` definition (like the one above).
        -}

    {-
        Type `Tree (a -> b)` means the left argument is not "a function", but
        rather a TREE OF FUNCTIONS.

        That means a value like

        ```
        Branch fl fr :: Tree (a -> b)
        ```

        literally contains potentially diferent functions in the left and right
        subtrees. So `<*>` has to say what it means to apply a function tree to
        a value tree. The zippy-style rule is:
        - apply the left function-subtree to the left value-subtree
        - apply the right function-subtree to the right value-subtree

        That's why you see "one function for each of lt and rt".

        Concrete example (why it's useful):
    -}
tf = Branch (Tip (+1)) (Tip (*10))      -- Tree of functions
tx = Branch (Tip 3)    (Tip 4)          -- Tree of values

val ta = tf <*> tx
-- Branch (Tip 4) (Tip 40)


{-
    If you "expected the same function for the whole tree", that corresponds to
    Functor, or to using `pure` to lift a single function into a singleton 
    function-tree:

    ```Haskell
    fmap (+2) tx
    -- equivalent to
    pure (+2) <*> tx
    ```

    So:
    - Same function everywhere: `fmap f t` (or `pure f <*> t`)
    - Possibly different functions in different parts: `tf <*> tx` where
      `tf :: Tree (a -> b)` is a tree of functions.

    This is exactly the same idea as `ZipList`: a container can hold multiple
    functions, and `Applicative` defines how they line up with values


            >>> (+) <$> [1, 2, 3] <*> [4, 5, 6]
            [5,6,7,6,7,8,7,8,9]

            The Applicative instance of ZipList applies the operation by pairing up the elements, analogous to zipWithN

            >>> (+) <$> ZipList [1, 2, 3] <*> ZipList [4, 5, 6]
            ZipList {getZipList = [5,7,9]}

            >>> (,,,) <$> ZipList [1, 2] <*> ZipList [3, 4] <*> ZipList [5, 6] <*> ZipList [7, 8]
            ZipList {getZipList = [(1,3,5,7),(2,4,6,8)]}

            >>> ZipList [(+1), (^2), (/ 2)] <*> ZipList [5, 5, 5]
            ZipList {getZipList = [6.0,25.0,2.5]}
-}




{-
    It depends entirely on how you define `(<*>)` for `Tree`.

    With the common lawful choice that goes with `pure = Tip`, you usually use
    these rules (broadcast + zip-at-the-top):

    ```Haskell
    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    (Tip f)        <*> t            = fmap f t
    (Branch fl fr) <*> Tip x        = Branch (fl <*> Tip x) (fr <*> Tip x) 
    (Branch fl fr) <*> Branch lt rt = Branch (fl <*> lt)    (fr <*> rt)
    ```

    What this means when shapes differ:


    CASE A: function tree is smaller (`Tip f`)

        Then the same function is applied to the entire value tree:

        ```Haskell
        Tip (+2) <*> Branch (Tip 3) (Tip 4)
        = Branch (Tip 5) (Tip 6)
        ```


    CASE B: value tree is smaller (`Tip x`)

        Then the single value is duplicated down the function tree, because the
        function tree contains potentially different functions in different
        branches:

        ```Haskell
        Branch (Tip (+1)) (Tip (*10)) <*> Tip 3
        = Branch (Tip 4) (Tip 30)
        ```

    
    CASE C: Both branch, but deeper mismatches

        At each `Branch`, it tries to align left-with-left and right-with-right,
        and if it hits a `Tip` on one side earlier, it uses (A) or (B) locally.

        ```Haskell
        tf = Branch (Tip (+1)) (Branch (Tip (*2) (Tip (*3))))
        tx = Branch (Branch (Tip 10) (Tip 20)) (Tip 5)

        tf <*> tx
        = Branch ((Tip (+1)) <*> Branch (Tip 10) (Tip 20))
                ((Branch (Tip (*2)) (Tip (*3))) <*> Tip 5)
        = Branch (Branch (Tip 11) (Tip 21))
                (Branch (Tip 10) (Tip 15))
        ```             <-- Interesting, because once you hit a dead-end, one can easily imagine as if this dead-end 
                            itself is like a NEW INDIVIDUAL TREE itself...


-}

------ ------       ------ ------                ------ ------ ------ ------

{-

Yes. In mathematics these show up constantly—usually under the names **inductive/recursive definitions** (and, on the dual side, **coinductive** ones).

## Inductive “datatypes” are standard in math

Your Haskell type

```haskell
data Tree a = Tip a | Branch (Tree a) (Tree a)
```

corresponds directly to the mathematical definition:

> `Tree(A)` is the **smallest set** such that
>
> 1. for every `a ∈ A`, `Tip(a) ∈ Tree(A)`
> 2. if `t1, t2 ∈ Tree(A)`, then `Branch(t1, t2) ∈ Tree(A)`.

That’s exactly “one constructor is a base case; one constructor references itself”.

You see the same pattern for:

* **Naturals (Peano axioms)**: `0` and `S(n)`
* **Lists**: `[]` and `x : xs`
* **Binary trees, rose trees, tries**, etc.

## Two core math principles drop out immediately

### Structural induction (proofs)

To prove a property `P(t)` for all trees, it suffices to show:

1. `P(Tip(a))` holds for all `a`
2. `P(Branch(l,r))` holds assuming `P(l)` and `P(r)` hold

That is literally the math version of “pattern match on constructors”.

### Structural recursion (definitions)

To define a function on trees, it suffices to define:

* what it does on `Tip a`
* how it combines results from the subtrees on `Branch l r`

This is the mathematical justification for folds/catamorphisms.

## Category theory view (why this is so “canonical”)

A lot of these recursive datatypes are **fixed points** of a shape functor.

For `Tree a`, the “shape equation” is:

[
X \cong A ;+; X \times X
]

read as: “a tree is either a tip holding an `A`, or a pair of trees”.

This is the same “sum of products” structure behind algebraic data types.

## Are they common outside pure math?

Very:

* **File systems / org charts** (trees)
* **ASTs (parsing, compilers)**: expressions are inductively defined syntax trees
* **JSON/XML/HTML DOM** (tree-structured data)
* **Decision trees, hierarchical clustering, phylogenetic trees**
* **Proof trees** in logic, derivations in type systems

## One more important distinction: inductive vs coinductive

* **Inductive** definitions describe **finite** structures (finite trees).
* **Coinductive** definitions describe potentially **infinite** structures (streams, infinite trees). In Haskell, laziness lets you work with these naturally; in math, this is treated via coinduction/final coalgebras.

If you want, paste your exact `Tree` and the `<*>` you’re using, and I can state the corresponding “math object” it models (zipping vs broadcasting vs nondeterminism-style), because different `Applicative`/`Monad` instances correspond to different mathematical semantics.

-}
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




{-
PURE EXAMPLES

-- pure 3 :: Maybe Int
pure 3
-- returns Just 3


-- pure 3 :: [Int]
pure 3
-- returns [3]


-- pure "Hello" :: IO String
pure "Hello"
-- returns an IO action that produces "Hello"


-- pure 3 :: Int -> Int
pure 3 5
-- returns 3 (ignores the input 5)
-}