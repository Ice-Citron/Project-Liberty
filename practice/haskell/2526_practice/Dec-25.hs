import Control.DeepSeq

{-
    1. What are Peano Naturals?

    Peano arithmetic is a way of defining natural numbers (0, 1, 2...) using 
    only two symbols:

    1. `Z` (Zero): The number 0.
    2. `S` (Successor): A function that means "+1".

    So, the numbers are built like nesting dolls:
        0 = Z
        1 = S Z (Successor of 0)
        2 = S (S Z) (Successor of 1)
        3 = S (S (S Z)) (Successor of 2)

    In Haskell code:        
-}
data Nat = Z | S Nat deriving Show

{-

    ---

    2. How the Code Works (`Num` Instance)

    To make Haskell treat these nested dolls like normal numbers (so you can 
    write `5` instead of `S (S (S (S (S Z)))))`, you need to implement the `Num`
    typeclass.

    A. The Bridge: `fromInteger`

    This function teaches Haskell how to translate a regular number (like `3`)
    into your `Nat` type.
-}

fromInteger' :: Integer -> Nat
fromInteger' 0 = Z
fromInteger' n = S (fromInteger' (n - 1))

{-
    B. Addition

    We define addition recursively. To calculate m + n:
    - Base Case: If m is Zero, the answer is just n. (0 + n = m)
    - Recursive Case: If m is (S m'), we peel off the S, add m' + n, and then 
      put the S back on the outside.
      - Math Logic: `(1 + m') + n = 1 + (m' + n)`
-}

(+) Z n = n
(+) (S m) n = S (m Main.+ n)

{-
    C. Multiplication (`*`)

    Multiplication is just repeated addition.
    - Base Case: 0 * n = 0 (Z)
    - Recursive Case: (m + 1) * n = n + (m * n)
-}

(*) Z     n = Z
(*) (S m) n = n Main.+ (m Main.* n)

-- Basically: "Add `n` to the result `m` times."

{-
    ghci> (fromInteger' 5) Main.+ (fromInteger' 3)
    S (S (S (S (S (S (S (S Z)))))))

        3. Why 5 + 3 :: Nat Works
            When you type 5 + 3 :: Nat into GHCi, a fascinating translation happens hidden from view:

            Translation: Haskell sees the literal 5 and 3. Since you said they are Nat, it secretly calls fromInteger on them.

            Haskell

            (fromInteger 5) + (fromInteger 3)
            Conversion: It runs your fromInteger code.

            Haskell

            (S (S (S (S (S Z))))) + (S (S (S Z)))
            Execution: It runs your recursive (+) function.

            Haskell

            S (S (S (S (S (S (S (S Z)))))))) -- Which is 8
            Display: If you derived Show, it prints that chain of Ss. If you wrote a custom Show instance using toInteger, it prints "8".
-}



------ ------       ------ ------ ------ ------         ------ ------ ------ 
-- Deriving for Free

{-
    Here's something magical about newtypes. Normally you can only derive a
    handful of typecassess (`Eq`, `Ord`, `Show`, etc.). But with newtypes, you
    can derive any typeclass that the underlying type has:
-}
newtype Radians = Radians Double 
  deriving (Eq, Ord, Fractional, Floating)

{-
    Why does this work? Because the implementation is trivial--just unwrap, do
    the operation, rewrap:
-}

instance Num Radians where
  Radians x + Radians y = Radians (x Prelude.+ y)

{-
    Since the wrapper doesn't exist at runtime anyway, GHC can just reuse the
    `Double` operations directly.

---

    The Real Power: Multiple Instances

    Here's where newtypes shine. Sometimes a type has multiple valid ways to 
    implement a typeclass, and Haskell can't pick one.

    Consider integer with the `Monoid` typeclass (which needs an "empty" value
    and a combining operation):

    - `(0, +)` works: `0 + x = x`, and `+` is associative
    - `(1, *)` works: `1 * x = x`, and `*` is assosciative
-}


{-
    1. What is a Monoid?

    A Monoid is a fancy mathematical name for a very simple concept. In 
    programming terms, a Type is a `Monoid` if it has two specific things:

    1. A Combining Function (Operator): A way to smash two values together to
       get a new value. (Often written as `<>`).
    2. An Identity Value (Empty): A special "neutral" value that, when smashed
       with anything else, changes nothing. (Often written as `mempty`).

    ---

        In math, a monoid is a set with a binary operation (like addition or
        multiplication) that is associative (order of grouping doesn't matter)
        and contains an identity element (an element that doesn't change others)
        , making it a basic algebraic structure, like groups but without 
        requiring inverses for all elements, common for things like number sets
        with addition, matrices with multiplication, or strings with 
        concatenation.

    ---

    The "Laws" (Rules):
    - Associativity: `(a <> b) <> c` must equal `a <> (b <> c)`.
    - Identity: `x <> empty` must equal `x` (and `empty <> x` must equal `x`).

    Example 1: Lists (`[a]`)
    - Combining: `++` (Appending). `[1, 2] ++ [3, 4]` = `[1, 2, 3, 4]`.
    - Empty: `[]` (Empty list).
    - Check: `[1, 2] ++ []` is `[1, 2]`
    - Conclusion: Lists are a Monoid

    Example 2: Strings (`String`)
    - Since Strings are just lists of characters, they are also Monoids 
    (`"Hello" ++ ""` is `"Hello"`).

    ---

            2. Is Monoid an existing type?
            No, Monoid is not a Type (like Int or String). It is a Typeclass (like an Interface in Java).

            You don't have "a variable of type Monoid." Instead, you have "a variable of type Int, and Int has an instance of the Monoid typeclass."

            The definition looks like this:

            Haskell

            class Semigroup a => Monoid a where
                mempty  :: a
                mappend :: a -> a -> a
                -- mappend is often written as (<>)
            3. The Problem with Integers
            This is the core of your confusion. For Lists, there is really only one obvious way to be a Monoid (stick them together).

            But for Int, there are two equally valid ways to follow the rules, and they conflict with each other.

            Option A: Addition
            Combining: +

            Empty: 0

            Check: 5 + 0 = 5. (1 + 2) + 3 = 1 + (2 + 3).

            This is a valid Monoid.

            Option B: Multiplication
            Combining: *

            Empty: 1

            Check: 5 * 1 = 5. (2 * 3) * 4 = 2 * (3 * 4).

            This is ALSO a valid Monoid.

            Haskell's Dilemma: If you wrote mappend 2 3, what should Haskell do?

            Should it return 5 (Addition)?

            Should it return 6 (Multiplication)?

            The compiler cannot read your mind. Because there is ambiguity, Haskell refuses to give Int a default Monoid instance.
    
-}



------ ------       ------ ------ ------ ------         ------ ------ ------ 
------ ------       ------ ------ ------ ------         ------ ------ ------ 

-- 4. The Solution: `newtype` Wrappers

{-
    Since `Int` can't choose, we create "Wrappers" (using `newtype`) to assign
    a specific personality to that integer.

    The `Sum` Wrapper

    - This wrapper says: "I am an Integer, but I behave like an Adder."
-}

newtype Sum a = Sum a deriving Show

instance Num a => Semigroup (Sum a) where       -- can't define Monoid because of GHC8.0 superclass...
    Sum x <> Sum y = Sum (x Prelude.+ y)

-- Usage
a = Sum 2 <> Sum 3    
-- Result: Sum 5


{-
    The `Product` Wrapper

    This wrapper says: "I am an integer, but I behave like a Multiplier."
-}
newtype Product a = Product a deriving Show

instance Num a => Semigroup (Product a) where
    Product x <> Product y = Product (x Prelude.* y)



{-
    Summary
    - Monoid = Anything that can be combined and has an empty state.
    - Ambiguity = Integers can be combined by `+` OR `*`.
    - Newtype = A label you put on integer to tell Haskell which combining 
      strategy to use (`Sum` for `+`, `Product` for `*`).
-}

------ ------       ------ ------ ------ ------         ------ ------ ------ 
{-
    `<>` is the official, standard operator for combining things in Haskell.

    It's the actual method defined in the Semigroup typeclass (which is the 
    parent of Monoid).

            1. The Modern Hierarchy
            In modern Haskell, the definition looks like this:

            Haskell

            class Semigroup a where
                (<>) :: a -> a -> a  -- The required combining operator

            class Semigroup a => Monoid a where
                mempty  :: a
                mappend :: a -> a -> a
                -- mappend is just a default alias for (<>)
                mappend = (<>)
            2. mappend vs <>
            mappend: This is the "old school" name. You will see it in older tutorials. It is clunky to write (mappend x y).

            <>: This is the modern, preferred operator. It is an infix operator, meaning it sits between the two arguments (just like + or *).

            3. Why is it cool?
            Because <> works on everything that is a Monoid, not just your Sum types. It is the "Universal Plus Sign."

            Haskell

            -- 1. Summing Numbers
            Sum 2 <> Sum 3 
            -- Result: Sum 5

            -- 2. Multiplying Numbers
            Product 2 <> Product 3
            -- Result: Product 6

            -- 3. Appending Lists (Same as ++)
            [1, 2] <> [3, 4]
            -- Result: [1, 2, 3, 4]

            -- 4. Appending Strings
            "Hello" <> " " <> "World"
            -- Result: "Hello World"

            -- 5. Merging Maps (combines values if keys collide!)
            Map.fromList [("a", 1)] <> Map.fromList [("a", 2)]
            -- Result: Map.fromList [("a", 3)] (It summed the 1 and 2!)
            

    Summary
    - Whenever you see `<>`, just read it as "Combine these two things." It is
      the exact same logic as `Sum 2 <> Sum 3` applied to everything else.
-} 


{-
            The Real Power: Multiple Instances
            Here's where newtypes shine. Sometimes a type has multiple valid ways to implement a typeclass, and Haskell can't pick one.
            Consider integers with the Monoid typeclass (which needs an "empty" value and a combining operation):

            (0, +) works: 0 + x = x, and + is associative
            (1, *) works: 1 * x = x, and * is associative
            (minBound, max) works: max minBound x = x
            (maxBound, min) works: min maxBound x = x

            All four are mathematically valid monoids! Haskell's solution is to define none of them for Int, and instead provide newtypes:
            haskellnewtype Sum = Sum Int
            newtype Product = Product Int
            Now Sum can have the (0, +) instance, and Product can have the (1, *) instance. No ambiguity—you choose which behavior you want by choosing which newtype to use.


            The Down Trick
            Data.Ord provides a clever newtype:
            haskellnewtype Down a = Down a

            instance Ord a => Ord (Down a) where
            compare (Down x) (Down y) = compare y x  -- note: y x, not x y!
            It just flips the comparison. Why is this useful? Sorting:
            haskellsort [4, 2, 5, 8, 3]        -- [2, 3, 4, 5, 8]
            sortOn Down [4, 2, 5, 8, 3] -- [8, 5, 4, 3, 2]
            You get reverse sorting without needing a separate function—just by wrapping in a newtype that inverts the ordering.
-}



------ ------       ------ ------ ------ ------         ------ ------ ------ 
-- The Laziness Gotcha

-- One subtle difference between `data` and `newtype`:
data Foo = Foo Int    deriving Show
newtype Bar = Bar Int deriving Show

f !(Foo _) = ()
g !(Bar _) = ()

{-
    `f (Foo undefined)` returns `()`--the bang pattern forces the `Foo` 
    constructorm but not the `Int inside.`

    `g (Bar undefined)` crashes--there is no `Bar` constructor at runtime, so
    forcing it forces the `undefined` directly.

    This rarely matter in practice, but it's worth knowing that newtypes are
    "more strict" in this sense.


    <-- Seems to be teaching about Runtime Erasure and how it changes the
        meaning of "Strictness"

        Lesson: `newtype` constructors are fake.


                This is teaching you about Runtime Erasure and how it changes the meaning of "Strictness."

                The lesson is: newtype constructors are fake.

                Here is the breakdown of why the crash happens:

                1. The data Case (The "Box")
                Haskell

                data Foo = Foo Int
                f !(Foo _) = ()
                When you use data, you are creating a real "box" in memory.

                The Value: Foo undefined is a real box containing a ticking time bomb (undefined).

                The Bang (!): This tells Haskell: "Check if the Box exists." (This is called Weak Head Normal Form).

                The Result: Haskell looks at Foo undefined. It sees a valid Foo box. It says "Okay, the box is real," and stops. It does not open the box.

                Outcome: You are safe. No crash.

                2. The newtype Case (The "Ghost")
                Haskell

                newtype Bar = Bar Int
                g !(Bar _) = ()
                When you use newtype, the constructor Bar is a lie. It exists only for the compiler to check types. At runtime, the "wrapper" is completely deleted.

                The Value: Bar undefined. At runtime, the Bar wrapper vanishes. This value is literally just undefined.

                The Bang (!): This tells Haskell: "Check if the value exists."

                The Result: Since there is no box to protect it, Haskell touches the undefined directly.

                Outcome: CRASH.

                The Visual Analogy
                data: You are handed a closed cardboard box labeled "Foo". The bang pattern asks: "Is this a box?" You say "Yes." You don't check if the cat inside is dead or alive.

                newtype: You are handed the cat directly (but the invoice says "Bar"). The bang pattern asks: "Is this a cat?" You poke it. It explodes.

                Why does this matter?
                It usually doesn't, but it proves that newtype is transparent. When you force a newtype to be evaluated, you are instantly forcing the underlying data to be evaluated too, because there is no "layer" in between to stop you.
    
    ---

    Summary
    - Newtypes are Haskell's way of giving you type safety without runtime cost.
      Use them when:
      1. You want to distinguish values that have the same representation 
         (Radians vs Degrees, USD vs GBP).
      2. You need multiple typeclass instances for the same underlying type 
         (Sum vs Product)
      3. You want to modify existing typeclass behavior (Down for reverse 
         ordering)

      The trade-off is that you can only wrap a single value in a single 
      constructor--but that's exactly what enables the zero-cost abstraction.
-}


------ ------       ------ ------ ------ ------         ------ ------ ------ 

-- Hierarchy of "how evaluated is this data?" in Haskell:

{-
    1. The Hierarchy
      1. Thunk (Unevaluated):
        - A completely lazy promise. Haskell hasn't even looked at it yet.
        - Example: `1 + 2` (before usage).
      2. Weak Head Normal Form (WHNF):              <-- `data`
        - The Default. Haskell stops here most of the time.
        - It evaluates just enough to see the outermost constructor (the "Head")
          . It does not look inside.
        - Example: `Just (1 + 2)`
          * We know it is a `Just`.
          * We don't know that it contains `3` yet. The inside is still a thunk.
      3. Normal Form (NF):
        - The "String" one. Fully evaluated.
        - It recursively forces every single thunk inside the structure. "Deep"
          evaluation.
        - Example: `Just 3`
          - We know it is a `Just`.
          - We know the inside is `3`.


    ---

        2. Concrete Example
        Let's look at the list xs = [1+1, 2+2].

        Stage 1: Thunk

        xs is just a pointer to code that will generate a list.

        Memory: ?

        Stage 2: Weak Head Normal Form (WHNF)

        You forced the list (e.g., by checking null xs or using seq).

        Haskell evaluates the first "cons" cell (:).

        Memory: (:) (thunk 1+1) (thunk rest_of_list)

        Insight: We know it is not empty. We do not know the numbers inside are 2 and 4.

        Stage 3: Normal Form (NF)

        You deeply forced the list (e.g., using deepseq or printing it to the console).

        Memory: (:) 2 ((:) 4 [])

        Insight: No thunks left anywhere. The data is "pure".
-}



{-
    3. How to achieve them

    - To get WHNF: Use `seq` or a Bang Pattern (`!`).
-}
x :: [Integer]
x = [1..1000000]
xa = x `seq` ()     -- x is now in WHNF (we know it starts with 1, but we haven't built the rest)


    -- To get Normal Form: Use `deepseq` (from `Control.DeepSeq`) or the `force`
    -- function

-- import Control.DeepSeq

y :: [Integer]
y = [1..1000000]
ya = y `deepseq` () -- x is now in Normal Form (the whole list is built in memory)


{-
    `()` is the unit value, and `()` (the same spelling) is also the unit type.
    - Type: `()`
    - Only value of that type: `()`

    It's like "there is exactly one trivial value", similar to `void` in some
    languages (but with an actual value), or like a 0-field struct.


                    Why does x \seq` ()` make sense?

                    seq has type:

                    seq :: a -> b -> b


                    and it behaves like:

                    evaluate the first argument to WHNF

                    then return the second argument

                    So:

                    x `seq` ()


                    means: “force x to WHNF, then return ()”.

                    () is used because:

                    it’s a simple return value you don’t care about

                    it doesn’t carry information

                    it’s cheap

                    It is not an empty container. It’s just a trivial value used as a “token” result.

                    What does WHNF mean here?

                    For a list:

                    WHNF of [] is []

                    WHNF of (h:t) is a cons cell (h:t) (but h and t may still be unevaluated)

                    So:

                    x = [1..1000000]

                    x \seq` ()forcesxenough to know whether it’s[]or(:)`.

                    For [1..], forcing to WHNF makes it produce the first cons cell, which in turn determines the first element is 1 and the tail is the thunk for [2..].

                    Why return () at all?

                    Because seq must return something (b). Often you’re using seq only for its forcing effect, so you return () as a dummy value.

                    Equivalent pattern:

                    let !_ = x   in ()


                    (using a bang pattern forces x to WHNF, then yields ()).

                    Contrast with deepseq

                    deepseq :: NFData a => a -> b -> b forces the first argument all the way (normal form), then returns the second.

                    So y deepseq () means: “fully build/evaluate y, then return ().”
-}
------ ------       ------ ------ ------ ------         ------ ------ ------ 
------ ------       ------ ------ ------ ------         ------ ------ ------ 
------ ------       ------ ------ ------ ------         ------ ------ ------ 
