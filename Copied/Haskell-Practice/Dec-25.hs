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