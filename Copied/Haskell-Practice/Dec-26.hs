{-
    ```Haskell
    instance Functor (Either e) where
      fmap f (Left x)  = Left x         -- don't touch the Left
      fmap f (Right x) = Right (f x)    -- only transform the right
    ```
-}



------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------

{-

    This is one of the coolest "lightbulb moments" in Haskell.

    To understand this, you have to stop thinking of Functors as "containers" 
    (like Lists or Boxes) and start thinking of them as "CONTEXTS".

    1. THE SHORT ANSWER

    For functions, `fmap` is literally just FUNCTION COMPOSITION (`.`).
    - Normal `fmap`: "Apply this function to the value inside the box."
    - Function `fmap`: "Apply this function to the RESULT of the previous 
        function."

    ---

    2. THE TYPE MATCHING (THE PROOF)

    Let's look at the type signature of `fmap` and swap in the Function type to
    see why they match perfectly.

    STANDARD FUNCTOR DEFINITION:
    ```Haskell
    fmap :: (a -> b) -> f a -> f b
    ```

    Now substitute `f` with `((->))`

    

-}

{-
        |:~8a.`~888a:::::::::::::::88......88:::::::::::::::;a8~".a888~|
        |::::~8a.`~888a::::::::::::88......88::::::::::::;a8~".a888~:::|
        |:::::::~8a.`~888a:::::::::88......88:::::::::;a8~".a888~::::::|
        |::::::::::~8a.`~888a::::::88......88::::::;a8~".a888~:::::::::|
        |:::::::::::::~8a.`~888a:::88......88:::;a8~".a888~::::::::::::|
        |::::::::::::::::~8a.`~888a88......88;a8~".a888~:::::::::::::::|
        |:::::::::::::::::::~8a.`~888......88~".a888~::::::::::::::::::|
        |8888888888888888888888888888......8888888888888888888888888888|
        |..............................................................|
        |..............................................................|
        |8888888888888888888888888888......8888888888888888888888888888|
        |::::::::::::::::::a888~".a88......888a."~8;:::::::::::::::::::|
        |:::::::::::::::a888~".a8~:88......88~888a."~8;::::::::::::::::|
        |::::::::::::a888~".a8~::::88......88:::~888a."~8;:::::::::::::|
        |:::::::::a888~".a8~:::::::88......88::::::~888a."~8;::::::::::|
        |::::::a888~".a8~::::::::::88......88:::::::::~888a."~8;:::::::|
        |:::a888~".a8~:::::::::::::88......88::::::::::::~888a."~8;::::|
        |a888~".a8~::::::::::::::::88......88:::::::::::::::~888a."~8;:|
-}

------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------


{-
    because the arrow `->` is usually "invisible" infrastructure, but here we 
    are treating it like a piece of data.

    Here is the breakdown of what's happening in GHCi and the type system.

    1. Why GHCi failed you

    You typed `:type (->)`. Haskell yelled at you because `(->)` is not a value. 
    You can't hold it in your hand. You can't print it.
    - `5` is a value.
    - `Int` is a Type.
    - `->` is a Type Constructor (a function that builds types).
 
    To see it in GHCi, you must use `:kind` (which asks "What kind of type is 
    this?"), or `:info`.

    ```Haskell
    Prelude> :kind (->)
    (->) :: * -> * -> *
    ```

    Translation: "The arrow takes TWO types (an Input type and an Output type)
    and creates a new concrete Type."

    ---

    2. THE "INFIX" SECRET

    You are used to seeing the arrow in the middle (infix notation):

    ```Haskell
    Int -> Bool
    ```

    But just like `1 + 2` can be written as `(+) 1 2`, the arrow is actually a
    prefix operator at the type level!

    THESE TWO LINES ARE 100% IDENTICAL TO THE COMPILER:

    ```Haskell
    Int -> Bool         -- The way humans write it
    (->) Int Bool       -- The way the compiler sees it
    ```

    - `Int` is the first argument (Input).
    - `Bool` is the second argument (Output).

    ---

    3. Understanding `((->) x)`

                -- Seems like `(x ->)` <=> `((->) x)`       <-- :kind == * -> *

    Now we get to the "partial application" part.

    If `(->)` takes TWO arguments, what happens if we only give it ONE?

    Let's say we give it the input type `Int` (or generic `x`), but we leave the
    output blank.

    ```Haskell
    (->) Int
    ```

    This is a "Type Function" waiting for one more argument.
    - If you give it `String`, it becomes `Int -> String`.
    - If you give it `Bool`, it becomes `Int -> Bool`.

    THIS IS THE "BOX"! In `Functor f`, `f` must be a "container" waiting for one
    item.
    - `[]` (List) is waiting for an item type to become `[Int]`.
    - `((->) Int)` (Function) is waiting for a result type to become 
      `Int -> String`.

    Therefore: `((->) x)` is the "Context" of a function that takes `x`.

    ---

    4. Proving it with Code (The "Wait for x" Trick)

    Let's implement `fmap` for functions without knowing it's composition, just
    by following the types.

    We want to write: `fmap f g`                            <-- ngl, i do kinda wonder in a lot of cases for haskell, could we just omit out the last or last-two or in fact more pattern matches... yet the function still works if we shove in more things? idk, just looking at some of the pure functions simplying f x = Just x into f = Just for example... makes me think this way, and maybe im confused, but i can't help but keep seeing a lot of examples that keep doing this tho yk...
                                                                <-- nevemind, this is just Eta Reduction... pretty annoying tbh...
    - `f :: a -> b` (The transformation, like `(+1)`)
    - `g :: x -> a` (The producer/container, like `(*2)`)
    - Goal: Return a new thing of type `x -> b`.

    ```Haskell
    -- Definition:
    fmap f g = \x -> ...
    -- We have an input 'x'.
    -- We have to produce a 'b'.

    -- Step 1: Run the producer 'g' with 'x' to get the 'a' out.
    let b = g x

    -- Step 2: Apply the transofrmation 'f' to 'a' to get 'b'.
    let output = f b

    -- Step 3: Return output.
    f b
    ```


    Combine it all:
    ```Haskell
    fmap f g = \x -> f (g x)
    ```

    Does `\x -> f (g x)` look familiar? That is the definition of Function 
    Composition `(.)`.

    ```Haskell
    (f . g) x = f (g x)
    ```


    5. Summary Trace

    So, when you write `fmap (+1) (*2)`:
    1. Functor View: "Take the (*2) container, and apply (+1) to the value 
       inside."
    2. Reality: "Take the (*2) function, run it, and feed the result into (+1)."
    3. Math: `(+1) . (*2)`          <-- this is eta-reduced too, something I need to get used to

    It's called the READER FUNCTOR because the functions are "reading" from a 
    shared source (`x`) before producing their value.
-}



{-
        |:~8a.`~888a:::::::::::::::88......88:::::::::::::::;a8~".a888~|
        |::::~8a.`~888a::::::::::::88......88::::::::::::;a8~".a888~:::|
        |:::::::~8a.`~888a:::::::::88......88:::::::::;a8~".a888~::::::|
        |::::::::::~8a.`~888a::::::88......88::::::;a8~".a888~:::::::::|
        |:::::::::::::~8a.`~888a:::88......88:::;a8~".a888~::::::::::::|
        |::::::::::::::::~8a.`~888a88......88;a8~".a888~:::::::::::::::|
        |:::::::::::::::::::~8a.`~888......88~".a888~::::::::::::::::::|
        |8888888888888888888888888888......8888888888888888888888888888|
        |..............................................................|
        |..............................................................|
        |8888888888888888888888888888......8888888888888888888888888888|
        |::::::::::::::::::a888~".a88......888a."~8;:::::::::::::::::::|
        |:::::::::::::::a888~".a8~:88......88~888a."~8;::::::::::::::::|
        |::::::::::::a888~".a8~::::88......88:::~888a."~8;:::::::::::::|
        |:::::::::a888~".a8~:::::::88......88::::::~888a."~8;::::::::::|
        |::::::a888~".a8~::::::::::88......88:::::::::~888a."~8;:::::::|
        |:::a888~".a8~:::::::::::::88......88::::::::::::~888a."~8;::::|
        |a888~".a8~::::::::::::::::88......88:::::::::::::::~888a."~8;:|
-}

------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------

{-
    In Haskell (specifically Point-Free style), code is a story about Pipelines:
    > "Connect the output pipe of `g` to the input pipe of `f`."


    1. Why `fmap` is "Eta-reduced by Default"
    ```Haskell
    fmap = (.)
    ```

    This feels like "cheating" because it looks like we forgot to write the
    code. But actually, we just relaized that we already have a word for this 
    concept.

    Imagine you are writing a dictionary definition for "Grandfather".
    - Explicit Definition (Data-Centric):
      > "A Grandfather is a person `z` such that if you take a person `x`, and 
      > `x`'s father is `y`, then `y`'s father is `z`." (This talks about the
      > people: x, y, z).
    - Implicit Definition (Process-Centric):
      > "Grandfather = Father of Father." (This talks about the relationship. No
      > specific people are mentioned).

    Haskell prefers the second one.
    - `fmap` (for functions) means "Apply after"/
    - `(.)` means "Apply after".
    - Therefore, `fmap = (.)`.


    2. It's "Pattern Recognition"

    You feel "unskilled" because your brain is still looking for the variables
    (`x`, `y`, `op`). You are mentally tracing the data flow.

    Haskell experts don't trace the data flow anymore. Thet look at the SHAPES
    OF THE PIPES.
    - You see: `length . myTester` and think "Where is the input?"
    - Haskell expert sees: A generic "Adapter" `(.)` that connects a "Producer"
      (`myTester`) to a "Consumer" (`length`). They know the input flows through
      automatically because the pipes are welded together.


    3. A Comforting Truth

    Even expert Haskellers often write the EXPLICIT version first!

    When defining complex functions, most people write:
    ```Haskell
    complexFunc x y z = ... (f x) ... (g y) ...
    ```

    And only after it works do they start deleting variables (Eta-reduction) to
    clean it up. It is a "refactoring" step, not necessarily how you write it 
    from scratch.


-}


f :: Int -> Int
f = (+2)            -- f x = (+2) x

------ ---- -------- --         -   -   ---         --  -----           ------


------ ---- -------- --         -   -   ---         --  -----           ------


{-
    MEMO TRIES (or "Tries").

    You are asking: "Can we represent a function `k -> v` as a data structure?"
    The answer is YES. The shape of that data structure depends entirely on what
    the input `k` is.

    1. 1-tail tree == a list

        This is literally an INFINITE LIST (or Stream)!

        ```Haskell
        -- The Data Representation of (Nat -> a)
        data Stream a = Cons a (Stream a)
        --                   ^     ^
        --            Value at 0   The rest of the function (1, 2, 3...)
        ```

        - Calling the function (`f 0`): You look at the head (a).
        - Calling the function (`f n`): You go to the tail and look up (n - 1).

        So yes, a function taking a Natural Number is a "1-tail tree" (a linear
        linked list).

    ---

    2. Your Guess: "Something like `Node (Tree a) a (Tree a)`"

    Verdict: Correct, if the input is a List of Booleans.

    Imagine a function `f :: [Bool] -> a`. The input is a sequence like `[Tree,
    False, True]`. How do you store all possible answers in a data structure? 
    You build an Infinite Binary Tree.

    ```Haskell
    -- The Data Representation of ([Bool] -> a)
    data BinTrie a = Node {
        val   :: a,         -- The value if the input list ends HERE
        left  :: BinTrie,   -- Where to go if the next bool is False
        right :: BinTrie    -- Where to go if the next bool is True
    }

    - The Root Node: Holds the value for `f []` (empty input).
    - Left Child: Holds the sub-function for inputs starting with `False`.
    - Right Child: Holds the sub-function for inputs starting with `True`.
    ```

    This is exactly the recursive structure you guessed: 
    `Node (Tree a) a (Tree a)`.

    
    3. The General Rule (The "Exponent")

    In math, function types are written as Exponents: `x -> a` is written as
     a^x.
    - If input `x` is `Bool` (2 values): a^Bool = a^2 = a * a. The data 
      structure is a Pair: `(a, a)`
      - (`fst` is the answer for False, `snd` is the answer for True).
    - If input `x` is `Nat` (Natural Numbers): a^Nat = a * a * a ... The data
      structure is a Stream (Infinite List).
    - If input `x` is `[Bool]` (List of Bools): The data structure is a Binary
      Tree.

    Summary
    - The "Function Arrow" `(->)` or `->` can be replaced by a `data` structure,
      but the SHAPE of that structure changes based on the INPUT TYPE.
    - This technique is used in Memoization. Instead of re-calculating the 
      function every time, we convert the function into this tree/list structure
      (call a Trie) and just look up the answer!
-}


{-
    Eta-reduction (often called "Point-Free Style".)

    Often we can just "delete" the last variable from both sides of the equation
    , and the function still works perfectly.

    Here is the breakdown of why thids works, using your `fmap` example, with 
    every Type explicit as requested.


    1. The Magic: "Currying"
    In Haskell, multi-argument functions do not exist.

    When you write:
    ```Haskell
    add :: Int -> Int -> Int
    add x y = x + y
    ```

    Haskell actually sees this:
    ```Haskell
    add :: Int -> (Int -> Int)
    add = \x -> (\y -> x + y)
    ```

    THE LOGIC: `add` is a function that takes ONE argument (`x`) and returns a
    NEW FUNCTION that is waiting for `y`.


    2. Deconstructing `fmap` for Functions

    Let's look at the `fmap` instance for functions again, but we will "reveal"
    the invisible variables one by one.

    Goal Definition:

    ```Haskell
    -- We want to define fmap for the Context ((->) r)
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    ```


    Version 1: The "Fully Explicit" Way (No skipping)

    Here, we explicitly name every single argument: `f` (the transform), 
    `g` (the producer), and `x` (the input to the producer).

    ```Haskell
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g x = f (g x)
    ```
    - `f` has type `(a -> b)`
    - `g` has type `(r -> a)`
    - `x` has type `r`
    - Result `f (g x)` 


    Version 2: Removing `x` (Eta Reduction)

    Notice that `x` is the last argument on the left, and the last thing applied
    on the right?

    Mathematically, if `h(x) = k(x)` for all `x`, then `h = k`.

    So we can delte `x`. But wait! On the right side, `f (g x)` is actually 
    `(f . g) x`.

    ```Haskell
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g
    ```
    - Where did `x` go? It is "hidden" inside the resulting function.
    - `f . g` returns a function that is waiting for an `x`.
    - Type Check: `f . g` has type `r -> b`. This matches the return type of 
      `fmap` (after it eats `f` and `g`).


    Version 3: Removing `g`?

    Can we go further? `fmap f g = (.) f g` (Prefix notation) Since `g` is at
    the end of both sides, we can delete it!

    ```Haskell
    fmap :: (a -> b) -> ((r -> a) -> (r -> b))
    fmap f = (.) f
    ```
    - This says: "`fmap` is a function that takes `f`, and conerts it into a
      composition operator waiting for a second function."

    
    Version 4: The "Galaxy Brain" (Removing `f`)

    `fmap f = (.) f` Since `f` is at the end of both sides...

    ```Haskell
    fmap :: ((a -> b) -> (r -> a) -> (r -> b))
    fmap = (.)
    ```
    - This is the definition you saw earlier: `fmap = (.)`.


    3. "Is `x` an additional variable that isn't declared?"

    In your example `fmap f g = \x -> ...`: Yes. `x` is the argument for the
    function that you are returning.

    If a function returns a function (e.g. `Int -> (Int -> Int))`, you can 
    either:
    1. Stop early and return the fucntion/lambda: `add x = \y -> x + y`.
    2. Or grab that argument immediately.

    Both define the exact same object.


    4. A Warning on "Omitting Pattern Matches"

    You asked: "could we just omit out the last... pattern matches... yet the 
    function still works if we shove in more things?"


          You must be careful with terminology here.Omitting Variables (Eta Reduction): Safe and common.myFunc x = otherFunc x  $\to$ myFunc = otherFuncThis is fine because you are just simplifying the pipeline.Omitting "Pattern Matches" (Cases): DANGEROUS.If you mean "ignoring constructors," that makes your function Partial.Example:Haskell-- BAD: Omitting the Empty list case
          head :: [a] -> a
          head (x:xs) = x
          If you shove in [], this crashes. Never omit cases unless you are 100% sure they are impossible.SummaryCurrying means f x y z is actually ((f x) y) z.Because arguments are applied one by one, you can often cancel out the last argument if it appears at the end of both sides.fmap f g = \x -> ... explicitly shows the "hidden" argument x that fmap produces.Simplifying f x = Just x to f = Just is perfectly valid because Just is a function (Constructor) that is waiting for an x.
-}



{-
        **YES! You absolutely can.**

        If you have a function that takes 20 arguments, and you just want to pass them all straight through to another function, you can delete **all 20 of them** from your definition.

        This is the ultimate power of **Currying**: A function is just a value. If `g` is a "20-argument function", and you say `f = g`, then `f` is now also a "20-argument function". You don't need to manually hand over the arguments one by one.

        ### 1. The "20 Variables" Example

        Imagine a horrifying function that takes 5 arguments (we'll stick to 5 to save space, but it works for 20, 100, or a million).

        ```haskell
        -- The complex library function
        complexMath :: Int -> Int -> Int -> Int -> Int -> Int
        complexMath a b c d e = a + b + c + d + e

        ```

        **The Long Way (Explicitly declaring variables):**
        You act as the middle-man, catching every ball and throwing it to `complexMath`.

        ```haskell
        myCopy :: Int -> Int -> Int -> Int -> Int -> Int
        myCopy a b c d e = complexMath a b c d e

        ```

        **The Short Way (Eta Reducing ALL of them):**
        You just say "I am `complexMath`."

        ```haskell
        myCopy :: Int -> Int -> Int -> Int -> Int -> Int
        myCopy = complexMath

        ```

        **Why this works:**
        Haskell sees `complexMath` not as "code waiting for 5 numbers," but as **a single object** (a function blob). You are just assigning that blob to a new name.

        ---

        ### 2. The Catch: They Must Be "Lineal Descendants"

        You can only delete the variables if they appear at the **very end** and in the **exact same order** on both sides.

        **✅ Works (Perfect Line):**

        ```haskell
        -- f x y z = g x y z
        f = g

        ```

        **❌ Fails (Out of Order):**

        ```haskell
        -- You flip x and y
        f x y = g y x 
        -- CANNOT reduce to 'f = g'. That would mean 'f x y = g x y'.

        ```

        **❌ Fails (Modified):**

        ```haskell
        -- You change z before passing it
        f x y z = g x y (z + 1)
        -- CANNOT reduce. 'f = g' would pass 'z' raw.

        ```

        **❌ Fails (Middle Argument):**

        ```haskell
        -- You use 'x' early
        f x y = g x y x
        -- CANNOT reduce. You need 'x' to appear twice.

        ```

        ### 3. "Pattern Matching" vs. "Passing the Blob"

        You asked about "omitting declaration in pattern match." This is a key distinction.

        * **Explicit:** `f x y = ...`
        This **is** a pattern match. You are saying: "Catch the first arg, name it `x`. Catch the second, name it `y`."
        * **Implicit (Eta Reduced):** `f = ...`
        This **skips** the pattern match entirely. You aren't "catching" anything. You are just pointing `f` to an existing function machine.

        ### Summary

        If your function is just a wrapper that passes **all** arguments straight through to another function unchanged, you can (and usually should) omit **all** of them.

        * **Long:** `logMessage level time user msg = printLog level time user msg`
        * **Short:** `logMessage = printLog`


      SUMMARY

      If your function is just a wrapper that passes ALL arguments straight
      through to another function unchanged, you can (and usually should) omit
      all of them.
            * **Long:** `logMessage level time user msg = printLog level time user msg`
            * **Short:** `logMessage = printLog`
-}

{-
          You are making a massive breakthrough here.

                Yes, `(r ->)` is a container.
                Yes, `(.)` is `fmap`.
                Yes, the "missing `x`" is the hardest part to see until your brain flips the switch.

                Here is the visualization that usually fixes the "Where did `x` go?" confusion.

                ### 1. The "Vending Machine" Analogy (The Container)

                You are right: `(r ->)` is a container `* -> *`.
                But unlike a `List` (which is a bucket of stuff), `(r ->)` is a **Vending Machine**.

                * **List `[a]`:** A bucket. You can look inside and see the `a` immediately.
                * **Function `r -> a`:** A locked box. The `a` is inside, but you can't get it until you insert a coin (`r`).

                **So what does `fmap` do?**
                `fmap` paints the item *inside* the machine without opening it.

                * **Before:** Vending machine `g` dispenses a **Snickers**.
                * **Action:** `fmap (coatInChocolate) g`.
                * **After:** You have a *new* Vending machine. If you put in a coin, it runs the old machine, gets the Snickers, coats it in chocolate, and gives you a **Choco-Snickers**.

                **Did we need the coin (`x`) to define the new machine?**
                No! We built the new machine just by bolting the "Chocolate Coater" onto the "Snickers Machine." The coin (`x`) only comes later when a customer arrives.

                ---

                ### 2. Visualizing the "Invisible X"

                You asked: *"Where is the x-var input coming from?"*

                The `x` comes from **The Future**.

                Let's look at `fmap f g`.

                * `f` is the Chocolate Coater (`a -> b`).
                * `g` is the Snickers Machine (`r -> a`).

                When you write `fmap f g`, you are creating a **Pipe**.

                ```haskell
                -- The "Pipe" Object
                (f . g)

                ```

                This pipe **is** a value. It sits on your desk. It is a real object.
                Does this pipe have an `x` inside it? **No.**
                Does this pipe *need* an `x`? **Yes**, eventually.

                **The Eta-Reduction Step:**

                1. **Explicit Mode:**
                "I am building a machine. If you give me a coin `x`, I will run `g` on `x`, then run `f` on that."
                ```haskell
                newMachine x = f (g x)

                ```


                2. **Implicit Mode (Eta-Reduced):**
                "I am building a machine that is composed of `f` attached to `g`."
                ```haskell
                newMachine = f . g

                ```



                **The "X" didn't disappear.** It just hasn't arrived yet. The definition `f . g` describes the *structure* of the machine, not the moment we turn it on.

                ---

                ### 3. Comparing the Definition side-by-side

                Let's prove `fmap` is `(.)` by forcing the `x` to appear.

                **Definition of `(.)` (Composition):**

                ```haskell
                (f . g) x = f (g x)

                ```

                **Definition of `fmap` for Functions:**
                We want to map the result of `g`.

                ```haskell
                (fmap f g) x = f (g x)

                ```

                **Look at them:**

                1. `(f . g)    x = f (g x)`
                2. `(fmap f g) x = f (g x)`

                Since they do the **exact same thing** to the input `x`, they are the **exact same machine**.

                Therefore, we can cancel the `x`:

                ```haskell
                f . g = fmap f g

                ```

                ### Summary

                * **The Container:** `(r ->)` is a container that holds a *future* value.
                * **The Map:** `fmap` modifies that future value by attaching a processing step to the end of the calculation.
                * **The X:** The `x` is the "trigger" that eventually runs the function. You don't need to mention the trigger to describe how the machine is wired together.
-}


{-
    SUMMARY
    - The Container: `(r ->)` is a container that holds a future value.
    - The Map: `fmap` modifies the future value by attaching a processing step
      to the end of the calculation.
    - The X: The `x` is the "trigger" that eventually runs the function. You
      don't need to mention the trigger to describe how the machine is wired 
      together.


-}

-- CHOCALATE COATER --> f
-- SNICKERS MACHINE --> g

    -- `fmap f g`


                -- SEEMS LIKE ANY BLOODY FUCKING THING THAT TAKES ONE INPUT, AND
                -- RETURNS ONE OUTPUT IS A FUNCTOR... OR HAS AN INSTANCE OF 
                -- FUNCTOR