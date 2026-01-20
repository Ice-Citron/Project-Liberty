
-- FUNCTORS

-- The Motivation: We Keep Writing the Same Function

data Tree a = Tip | Node (Tree a) a (Tree a) deriving Show
data Bush a = Leaf a | Fork (Bush a) (Bush a) deriving Show

-- Look at these three functions--they're almost identical:
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = (f x) : map f xs

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Tip            = Tip
mapTree f (Node lt x rt) = Node (mapTree f lt) (f x) (mapTree f rt)

mapBush :: (a -> b) -> Bush a -> Bush b
mapBush f (Leaf x)     = Leaf (f x) 
mapBush f (Fork lt rt) = Fork (mapBush f lt) (mapBush f rt)

{-
    What do they all have in common?
    - They transform every `a` into a `b` using the function `f`.
    - They PRESERVE THE STRUCTURE PERFECTLY--no elements are added, removed or
      reordered.

    This is the essence of "mapping." We want to abstract this pattern into 
    typeclass so we can write one generic function that works for lists, trees,
    bushes, and anything else that supports this operation.
-}

{-
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣤⢔⣒⠂⣀⣀⣤⣄⣀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⣴⣿⠋⢠⣟⡼⣷⠼⣆⣼⢇⣿⣄⠱⣄
                    ⠀⠀⠀⠀⠀⠀⠀⠹⣿⡀⣆⠙⠢⠐⠉⠉⣴⣾⣽⢟⡰⠃
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠈⢿⣿⣦⠀⠤⢴⣿⠿⢋⣴⡏⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⡙⠻⣿⣶⣦⣭⣉⠁⣿⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣷⠀⠈⠉⠉⠉⠉⠇⡟⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⣘⣦⣀⠀⠀⣀⡴⠊⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠈⠙⠛⠛⢻⣿⣿⣿⣿⠻⣧⡀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠫⣿⠉⠻⣇⠘⠓⠂⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⢶⣾⣿⣿⣿⣿⣿⣶⣄⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠹⣿⣿⣿⣿⣿⣿⣿⣧⠀⢸⣿⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠈⠙⠻⢿⣿⣿⠿⠛⣄⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡁⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠁⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⡆⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⣷⠂⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⠀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⣿⡀⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠇⠀⠀⠀⠀⠀⠀⠀
                    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠋⠀⠀⠀⠀⠀⠀⠀⠀
-}

------ ---- -------- --         -   -   ---         --  -----           ------

-- The Problem: What Type Would This Have?

{-
    If we try to write a generic `map`, what's its type?

    ```Haskell
    genericMap :: ??? => (a -> b) -> ??? a -> ??? b
    ```

    We need some type variable that itself takes a type parameter. Lists are 
    `[a]`, trees are `Tree a`, bushes are `Bush a`. The "container" part (
    `[]`, `Tree`, `Bush`) is what varies.

    This is where KINDS come in
-}



------ ---- -------- --         -   -   ---         --  -----           ------

-- Kinds: Types of Types

{-
    Values have types:

    ```Haskell
    True      :: Bool
    'a'       :: Char
    [1, 2, 3] :: Int
    ```

    Types have kinds. The kind `*` (pronounced "star" or "type") means "a 
    concrete type that can have values":

    ```Haskell
    Bool        :: *        -- Bool is a complete type
    Int         :: *        -- Int is a complete type
    [Int]       :: *        -- [Int] is a complete type
    Maybe Int   :: *        -- Maybe Int is a complete type
    ```

    But what about `Maybe` on its own? It's not a complete type--you can't have
    a value of type `Maybe`, only `Maybe Int`, or `Maybe String`, etc. `Maybe`
    is a TYPE CONSTRUCTOR that takes a type and produces a type:

    ```Haskell
    Maybe   :: * -> *           -- takes a type, returns a type
    []      :: * -> *           -- same: [Int], [Bool], etc.
    Tree    :: * -> *           -- same: Tree Int, Tree Bool, etc.
    Either  :: * -> * -> *      -- takes TWO types: Either String Int
    ```

    Think of it like functions, but at the type level. Just as `(+1)` takes an
    Int and returns an Int, `Maybe` takes a type and returns a type.
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

-- The Functor Typeclass

{-
    Now we can write our generic map:

    ```Haskell
    class Functor (t :: * -> *) where
      fmap :: (a -> b) -> t a -> t b
    ```

    The `t :: * -> *` says "t is a type constructor that takes one type 
    argument." This means `t` could be `[]`, `Maybe`, `Tree`, etc.

    `fmap` (functor map) is the generic version of all our specific map 
    functions.
-}

------ ---- -------- --         -   -   ---         --  -----           ------

-- Writing Functor Instances

{-
    For Tree:

    ```Haskell
    instance Functor Tree where
      fmap :: (a -> b) -> Tree a -> Tree b          -- equivalent to (<$>)
      fmap f Tip            = Tip
      fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)
    ```


    For Maybe:

    ```Haskell
    instance Functor Maybe where
      fmap f Nothing  = Nothing
      fmap f (Just x) = Just (f x)
    ```


    `data Either a b = Left a | Right b deriving Show`
    For Either: Here's where it gets interesting. `Either` has kind 
    `* -> * -> *` (it takes two type arguments). To make it a Functor, we 
    partially apply it:

    ```Haskell
    instance Functor (Either e) where
      fmap f (Left x)  = Left x         -- don't touch the Left
      fmap f (Right x) = Right (f x)    -- only transform the right
    ```

    `Eithere e` has kind `* -> *`, so it fits. The rule is: FUNCTOR ALWAYS WORK
    ON THE LAST TYPE PARAMETER
-}

------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------

-- EXAMPLES

{-
            Tracing Through Examples
            Example 1: fmap on Maybe
            haskellfmap (+1) (Just 5)
            = Just ((+1) 5)      -- apply f to the contents
            = Just 6

            fmap (+1) Nothing
            = Nothing            -- nothing to apply f to
            Example 2: fmap on Tree
            haskellfmap (*2) (Node Tip 3 (Node Tip 5 Tip))

            -- Start at the root: Node lt 3 rt where lt=Tip, rt=(Node Tip 5 Tip)
            = Node (fmap (*2) Tip) ((*2) 3) (fmap (*2) (Node Tip 5 Tip))
            = Node Tip 6 (fmap (*2) (Node Tip 5 Tip))

            -- Now recurse into the right subtree
            = Node Tip 6 (Node (fmap (*2) Tip) ((*2) 5) (fmap (*2) Tip))
            = Node Tip 6 (Node Tip 10 Tip)
            Every value got doubled. The tree structure is identical.
            Example 3: fmap on Either
            haskellfmap (+1) (Right 5)
            = Right ((+1) 5)
            = Right 6

            fmap (+1) (Left "error")
            = Left "error"       -- Left values are untouched
-}

------ ---- -------- --         -   -   ---         --  -----           ------
------ ---- -------- --         -   -   ---         --  -----           ------


{-

  This is one of the coolest "lightbulb moments" in Haskell.

  To understand this, you have to stop thinking of Functors as "containers" 
  (like Lists or Boxes) and start thinking of them as "CONTEXTS".

  1. THE SHORT ANSWER

  For functions, `fmap` is literally just FUNCTION COMPOSITION (`.`)

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

⣿⣿⣿⣿⣿⠟⠋⠄⠄⠄⠄⠄⠄⠄⢁⠈⢻⢿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⠃⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠈⡀⠭⢿⣿⣿⣿⣿
⣿⣿⣿⣿⡟⠄⢀⣾⣿⣿⣿⣷⣶⣿⣷⣶⣶⡆⠄⠄⠄⣿⣿⣿⣿
⣿⣿⣿⣿⡇⢀⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠄⠄⢸⣿⣿⣿⣿
⣿⣿⣿⣿⣇⣼⣿⣿⠿⠶⠙⣿⡟⠡⣴⣿⣽⣿⣧⠄⢸⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣾⣿⣿⣟⣭⣾⣿⣷⣶⣶⣴⣶⣿⣿⢄⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⡟⣩⣿⣿⣿⡏⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣹⡋⠘⠷⣦⣀⣠⡶⠁⠈⠁⠄⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣍⠃⣴⣶⡔⠒⠄⣠⢀⠄⠄⠄⡨⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣦⡘⠿⣷⣿⠿⠟⠃⠄⠄⣠⡇⠈⠻⣿⣿⣿⣿
⣿⣿⣿⣿⡿⠟⠋⢁⣷⣠⠄⠄⠄⠄⣀⣠⣾⡟⠄⠄⠄⠄⠉⠙⠻
⡿⠟⠋⠁⠄⠄⠄⢸⣿⣿⡯⢓⣴⣾⣿⣿⡟⠄⠄⠄⠄⠄⠄⠄⠄
⠄⠄⠄⠄⠄⠄⠄⣿⡟⣷⠄⠹⣿⣿⣿⡿⠁⠄⠄⠄⠄⠄⠄⠄⠄
ATTENTION CITIZEN! 市民请注意!
This is the Central Intelligentsia of the Chinese Communist Party. 您的 Internet 
浏览器历史记录和活动引起了我们的注意。 YOUR INTERNET ACTIVITY HAS ATTRACTED OUR 
ATTENTION. 因此，您的个人资料中的 11115 ( -11115 Social Credits) 个社会积分将打折。 
DO NOT DO THIS AGAIN! 不要再这样做! If you do not hesitate, more Social Credits 
( -11115 Social Credits )will be subtracted from your profile, resulting in the 
subtraction of ration supplies. (由人民供应部重新分配 CCP) You'll also be sent into 
a re-education camp in the Xinjiang Uyghur Autonomous Zone. 如果您毫不犹豫，更多的
社会信用将从您的个人资料中打折，从而导致口粮供应减少。 您还将被送到新疆维吾尔自治区的再教育营。
为党争光! Glory to the CCP!
-}