import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (forM)


m1 = Map.fromList [("a", 12), ("b", 30)]
m2 = Map.fromList [("b", False), ("c", True)]


-- | Merge two maps with potentially different value types
unionWithDiff :: (Ord k, Show k) => (a -> b -> c)         -- fBoth : when key in both maps
                                -> (a -> c)              -- fLeft : when key only in m1
                                -> (b -> c)              -- fRight: when key only in m2
                                -> Map.Map k a
                                -> Map.Map k b
                                -> IO ()
                                -- -> Map.Map k c
-- unionWithDiff fBoth fLeft fRight m1 m2 = Map.fromList $ do
unionWithDiff fBoth fLeft fRight m1 m2 = do
    -- STEP 1: Get all keys
    let allKeys = Set.union (Map.keysSet m1) (Map.keysSet m2)              -- Hint: Set.toList, Set.union, Map.keysSet
    print allKeys
    
    {-
    -- STEP 2: "for k in allKeys"
    k <- allKeys

    -- STEP 3: Lookups              -- incredible... so we use lookup here which then consequently tells the case...of block whether to call fBoth, fLeft or fRight!
    let maybeA = undefined
    let maybeB = undefined

    -- STEP 4: Compute the new value
    let newVal = case (maybeA, maybeB) of
        (Just a, Just b)   -> undefined
        (Just a, Nothing)  -> undefined
        (Nothing, Nothing) -> undefined
        _                  -> error "impossible"

    -- STEP 5: 
    return (k, newVal)
    -}
        -- <-- since (>>=) === join (fmap f ms)... this means stuff gets automaticaly stitched together!






{-
    FILL THIS OUT BEFORE EXAM


                SUMMARY CHEAT SHEET

                        Operation               List (`Data.List`)              Set (`Data.Set`)                Map (`Data.Maps`)
                        Convert from List       `id`                    
                        Check exists
                        Access Item
                        Insert
                        Delete
                        Filter
                        Map

                        CCA IDF M <-- convert from list // check exists // access item // insert // delete // filter // map



    ALSO FOCUS ON DOING QUIZZES PROVIDED BY GEMINI... THEY ARE ACTUALLY VERY USEFUL!
-}

{-
    0.
        PPT 3, 4, 5 LEFT


    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

       -- target 2021... need to use `zip` and `zipWith`
                -- need to also watch monday 4 hr lectures... likely will just pass it into AI... need to also practice with foldl and foldr

       -- 2022 is hard
       -- 2025 seem to be about parsers... or state monads

                `nub`
                \case
                [ParseTree]
                split2 and split3.          <-- custom PPQ function... ig we will learn this more in ppq... but pay attention to function re-use...

        == UNASSESSED EXERCISES NEXT
        

    2. still don't understand the `commands` and `commands'` functions from bottom of ApplicativeParsing.lhs
            -- A potential concern rn tbf... is that we are very good at implementation of interfaces rn... but not much good at
               actually using these functions and HOFs... but this should be fixable in the next 3 days

    3. need to practice with `do` notation for for-loops and LCs and in general...
            as in... also using the feature where we directly also engage with >> and `putStrLn` for instant debug!
                        have to get used to writing these monads myself in practice too...
                                        In Haskell, all of these are just Monads:
                                                        Looping? List Monad.
                                                        Null checks? Maybe Monad.
                                                        Error handling? Either Monad.
                                                        Async/Side-effects? IO Monad.
                                        https://gemini.google.com/app/fc93170cd93f6546
                                        https://gemini.google.com/app/56a9

    4. check again the command to disable overwrite mode... and to trigger the 80-columnth guideline...


    ADDITIONAL
        - check what was the interrim incredible monad technique useds
        - Claude chat - Four Approaches to LSystem Parsing
                            https://claude.ai/chat/9189b553-f13c-4cd2-90e2-cd9243d88d30




    THINGS HINTED TO BE NEEDED IN THE EXAM
    - `nub :: Eq a => [a] -> [a]`                       <-- `import Data.List` required
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id