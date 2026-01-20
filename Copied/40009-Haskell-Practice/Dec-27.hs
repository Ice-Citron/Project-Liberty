
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


{-

    Confusing the Value Level with the Type Level.

    1. Can't anything that takes 1 input be a Functor?

    Yes, but only at the Type Level.

    To be a `Functor`, a thing must have the "Kind" `* -> *`. This means it must
    be a Type Function (a Type Constructor) that takes One Type and returns One
    Type.
    - List (`[]`):
      - Takes `Int` --> Returns `[Int]`
      - Kind `* -> *`.
      - Verdict: YES, it's a valid container.
    - Maybe:
      - Takes `Bool` --> Returns `Maybe Bool`.
      - Kind: `* -> *`
      - Verdict: YES, it's a valid container.
    - Your Function Arrow `((->) r)`:
      - Takes `String` --> Returns `r -> String`.
      - Kind: `* -> *`
      - Verdict: YES, it's a valid container.

            Int:

            Takes nothing. It is already finished.

            Kind: *.

            Verdict: NO. It has no "slot" to put anything in. You can't have Int String.

            Either:

            Takes two types (Either String Int).

            Kind: * -> * -> *.

            Verdict: NO. It needs two inputs.

            Exception: If you fill one slot (Either String), it becomes * -> * and can be a Functor!
    

    The "Gotcha": A standard function like `f :: Int -> Int` takes 1 value and 
    returns 1 value. BUT `f` is a VALUE, NOT A TYPE. `f` cannot be a Functor. 
    The Type of `f` (which is `Int -> Int`) is a concrete thing (Kind `*`), so
    it cannot be a Functor either.

    Only the GENERIC STRUCTURE `((->) r)` is the Functor.

    ---


    "Most things by themselves are a function?" <-- Correction: At the Type 
    Level, most generic data structures (`List`, `Tree`, `Map`) behave like 
    functions because they "wait" for a type argument (`Int`, `String`) to 
    become real.
    - `List` is a function that turns `Int` into `[Int]`.
    - Because it fits that "1-input" shape (* -> *), it gets to be a Functor!




-}
------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------
--  MEGA CLARIFICATION: FUNCTORS ARE ONLY FOR TYPE CONSTRUCTORS... NOT THE ACTUAL
--  VALUES THEMSELVES...

-- values // concrete types // type constructors // function context // 
-- higher constructors // partially filled // grammar

-- FUNCTORS:
{-
    Type Constructors, Function Context, Partially Filled...

    which tbf... all 3 are basically TYPE CONSTRUCTOR once again... hence only
    TYPE CONSTRUCTORs can be Functors


                <-- partially filled = now that one hole is filled, they fit the shape!
-}
------ ---- -------- --         -   -   ---         --  -----           ------

