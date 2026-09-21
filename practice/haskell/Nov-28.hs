-- Plan during 10 minutes... Understand what structures are like... get a sense
-- of the structures, etc. before you start programming

{-
* We can tell that we have a sorting function involved... for tree because of...
* parts are going to build on top of each other

* reinventing the wheel is quite often <-- ignoring the functions that we've
    previously already written
-}

{-
* Can tell that at times... there's no need for recursive functions at all, such
    as for:
-}

freqCount :: HTree a -> Int
freqCount (Leaf c n) = n
freqCount (Node n _ _) = n

{-
* Spec says make sure you understand the tree diagram before starting
-}
merge :: HTree a -> HTree a -> HTree a
merge t1 t2 = Node(freqCount t1 + freqCount t2) (min t1 t2) (max t1 t2)

{-
* Don't forget about what you've just work... and also use your previous funcs
    even if you know that they don't work.
* All functions that we are expected to use were already shown during lectures.
    ... we shouldn't be searching browse at all times... hence for revision...
    please ensure all functions from lectures were thoroughly understood
-}

insert :: Ord a => a -> Bag a -> bag        -- O(log n)
insert x (Bag m) = Bag (Map.insertwith (+) x 1 m)
-- | Just n <- Map.lookup x m = 
{-
* If using the `lookup` function, please use the `Just` pattern matching scheme
-}

-- document is the package one?

fromList :: Ord a => [a] -> Bag a   -- O(n log n)
fromList = foldr insert empty

{-
IF YOU CAN'T GET SOMETHING AT THE START. JUST MOVE ON AND KEEP ON WITH IT

THE TEST WON'T RUN, BUT THE MARKERS AREN'T MARKING THE TEST, THEY ARE MARKING
WHAT THE FUNCTION ACTUALLY LOOKS LIKE

---

with regards to documentation. don't need to know everything, but for the things
highlighted in the PPT, those are what you should be focused on


- marker is interested in efficiency and number of function traversals here...
    so better to minimise instead
-}


------ ------ ------ ------     ------ ------ ------ ------     ------ ------ 
{-
    FOR SECTION 2, once again, THE BLOCK TELLS YOU WHAT YOU NEED TO KNOW. 
    Hence, make sure you understand that first.

-}

-- The spec was specific about using `Bag`... and hence if you didn't used
-- Bags, you would be penalised.    
occurrences :: Ord a => [a] -> [(a, Int)]
occurrences = Bag.toList . Bag.fromList


{-
    THE KEY is reading what the SPEC IS TELLING YOU
    DON'T JUST ASSUME THE IMPLICIT MEANING OF WHAT IT'S TRYING TO TRLL YOU, AS 
    WE DON'T EXPECT YOU TO FIGURE THINGS OUT, BUT JUST DIRECTLY IMPLEMENT   
-}
reduce :: [HTree] -> Htree a
-- Pre: The argument list is non-empty and sorted based on `Ord HTree`
reduce [t] = t
reduce (t1 : t2 : ts) = reduce (insert (merge t1 t2) ts)


{-
    The spec have also made clear mentions of how we should build buildTree...

    follow what the specs is saying... hence follow it, like about sort for 
    example... it says that `sort ....` will have the correct functionality...
    and is being very specific about it, hence just comply to it
-}
buildTree :: Ord a => [a] -> HTree a
-- Pre: The list is. non empty
buildTree = reduce . sort . map (uncurry Leaf) . occurences

{-
    Also check in spec ... and uses ghci to check how does `insert` work
-}



------ ------ ------ ------     ------ ------ ------ ------     ------ ------ 
{-
    "A precondition is that the given tree can encode each of the items in the
    list, i.e. each item has exactly one corresponding leaf somewhere in
    the tree."


    <-- misinterpretations seem to be happening...

    <-- intention: traverse the item per item on the list... instead of per
                unique item on the list
-}

    -- seem to introducing a new `forall a .` constraint, interestingly, 
    -- don't know what's going on here
encode :: forall a . Eq a => HTree a -> [a] -> Code     
-- Pre: The tree can encode each of the items in the list
encode = concatMap (encode' t [])
  where
    encode' :: HTree a -> Code -> a -> Code
    encode' (Leaf c1 _) pth c2
      | c1 == c2  = path
      | otherwise = []
    encode' (Node _ lt rt) path c =
      encode' lt (0 : path) c `max` encode' rt (1 : path) c

      -- wow, haven't seen this way of writing pattern matching before

      -- using Maybe instead... and was seen as a decent solution

    

    -- (0 :) <$> encode' lt c </> (1 :) <$>  encode' rt c
                                    -- WE MIGHT HAVE TO CEASELESSLY CHECK WITH CLAUDE, ON HOW CAN WE IMPLEMENT SUPER-MONAD FUNCTIONS INSTEAD, TO
                                    -- GET REALLY USED TO MONADS
decode :: HTree a -> Code -> [a]
-- Pre: The code is valid with respect to the tree
decode origT = decode' origT
  where 
    decode' (Leaf c _) bits = c : decode' origT bits
    decode' (Node _ lt rt) (0 : bits) = decode' lt bits
    decode' (Node _ lt rt) (1 : bits) = decode' rt bits
    decode' _ [] = []

            -- absolutely don't understand what's going on here... seems to be 
            -- plenty of destructuring and pattern-matching that's going on here
        



------ ------ ------ ------     ------ ------ ------ ------     ------ ------ 

convert :: [Int] -> Int
convert [] = 0
convert s@(x: xs) = x + 2 * convert xs

    {-
        The base case is the empty list, returning 0.

        The recursive case follows from the fact that x is the least significant 
        digit. convert xs (the recursive call) gives us the result for the tail 
        of the list; to get the result of the whole list we need to multiply by 
        2 (to "shift over" the digits) and add x (0 or 1).
    -}


------ ------ ------ ------     ------ ------ ------ ------     ------ ------ 

compressTree :: HTree Char -> [Int]
compressTree t = compress t []
  where
    bits :: Int -> Int -> Code
    bits 0 n = acc
    bits k n = let (n', bit) = divMod n 2 in bits (k - 1) n' . (bit :)

    compress :: HTree Char -> Code -> [Int]
    -- compress (Node _ lt rt) = 0 : compress lt ++ compress rt         -- THE ++s are EVIL. SO USE THIS FUNCTION COMPOSITION TECHNIQUE `(0 :) . ` TO REMOVE AND NUKE THEM!
    compress (Node _ lt rt) = (0 :) . compress lt . compress rt
    compress (Leaf c _) = (1 :) . bits asciiBits (ord c)

        -- check in :t about `divMod`'s function-type

    {-
        IF YOU WROTE 7... YOU LOST 3 WHOLE MARKS FOR THIS WHOLE ASCISS SECTION
        APPARENTLY... UNSURE WHY... I KNOW THAT IT'S DUE TO MAGIC NUMBERS BEING
        BAD... BUT COULD IT BE BECAUSE THEY WERE EXPECTING BINARY FORM
        YET WE JUST SHOVED IN 7?

        so it seems we were encouraged to use the variable `asciiBits` instead

        NEVER FUCKING USE MAGIC NUMBERS
    -}



    -- FUNCTIONS checked in :t and tested in ghci:
    --          --> take, drop, splitAt




-- HAVING OURSELVES A HELPER FUNCTION HERE <-- and it seems to be necessary too
        -- notice how he doesn't bother to define fromBinary... and just assumes
        -- that it works...
fromBinary :: [Int] -> Char
fromBinary bits = chr (sum (zipWith (*) (reverse bits) (map (2^) [0..])))


rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree (1 : bits) = fst . rebuildTree'
  where
    rebuildTree' :: [Int] -> (HTree Char, [Int])
    rebuildTree' (1 : bits) =
      let (ascii, bits') = splitAt ascii bits
      in (Leaf (fromBinary ascii) 0, bits')
    rebuildTree' (0 : bits) =
      let (lt, bits') = rebuildTree' bits
          (rt, bits'') - rebuildTree' bits'
      in  (Node 0 lt rt, bits'')



------ ------ ------ ------     ------ ------ ------ ------     ------ ------ 

-- import Data.Set (Set)
-- import Data.Set qualified as Set
reduce' :: Ord a => Set (HTree a) -> HTree a
reduce' ts
  | Set.size ts == 1 = Set.findMin ts
  | otherwise = let () in 



{-

- READ THE SPEC
- MOVE ON IF YOUR STUCK


-}