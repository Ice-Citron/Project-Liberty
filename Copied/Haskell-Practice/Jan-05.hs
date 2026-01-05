
{-
    HASKELL TIC-TAC-TOE
        -- looks like we just need to learn these wide range of custom variables from these Data.* libraries
                    -- uses `replicate` to create n^2 Empty cells.     `[100 | _ <- [1..10]]` === `do _ <- [1..10]; return 100`
                    -- `nub` in Data.List
                            -- very smart... if `nub` returns a singleton `[Taken 0]` or `[Taken X]` (which means that there's
                                no other pieces present... because nub only returns special pieces)... this means the whole line 
                                is that players' markers.
                    -- `elem :: (Foldable t, Eq a) => a -> t a -> Bool`
                    -- `any`, `all`
                    
                    ```Haskell
                    parsePosition :: String -> Maybe Position
                    parsePosition s = case words s of
                        [r, c] -> (,) <$> readMaybe r <*> readMaybe c              -- <-- (,) * <$> * <*> * ... is how one turns 2 elements into tuples!
                        _      -> Nothing                                          -- combines two `Maybe Int` into `Maybe (Int, Int)`... tbf probably could've used liftA2 instead
                    ```

                    -- row-major indexing: ...      `where idx = r * n + c`
                    -- used helper function `showcell`


                    ```Haskell
                    doParseAction :: String -> (String -> Maybe a) -> IO a
                    doParseAction errMsg parser = do
                        input <- getLine
                        case parser input of
                          Just x  -> return x
                          Nothing -> putStr errMsg >> doParseAction errMsg parser       <-- incredible... `>>` and recursion of `doParseAction` used...
                    ```
                    -- Key Technoque: Recursive retry pattern for IO with Maybe Validation
                        -- `getLine :: IO String`
                    
                    ```Haskell
                    takeTurn b p = do
                      putStr $ "Player " ++ show p ++ ", make your move (row col): "
                      doParseAction "Invalid move, try again: " tryParseAndMove
                      where
                        tryParseAndMove s = parsePosition s >>= \pos -> tryMove p pos b
                    ```
                    -- Key Technique: Chains `parsePosition` and `tryMove` with `>>=` (both return Maybe).


                    ```Haskell
                    playGame b p = do
                      prettyPrint b
                      newBoard <- takeTurn b p
                      if gameOver newBoard
                        then prettyPrint newBoard >> putStrLn ("Player " ++ show p ++ " has won!")
                        else playGame newBoard (swap p)
                    ```




                --------
                    DON'T understand
                    - prettyPrint
                        - explain to me what does mapM_ do... show example... and why `(.)` with intersperse?
                        - what does `intersperse` do?

                    - how to convert various types among each other
                        - forgot what does `fromIntegeral`
                        - i remember `show`...
                                    





-}



{-
    0.
        PPT 3, 4, 5 LEFT


    1. I think just focused very heavily on running through PPTs and PPQs... just look at all the techniques used by them and also 
       alternatives ones... and practice by asking claude for example to write out similar ones... SPEED

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
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id