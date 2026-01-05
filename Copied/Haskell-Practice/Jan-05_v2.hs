import Data.Foldable
import Data.Traversable         -- for importing forM and mapM
import Control.Monad
import Control.Applicative
import Data.List

------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----

                    -- further... interestingly there isn't really any difference between mapM and forM! because 
                            -- forM = flip mapM
                                        -- and the fact that `forM` mainly exists for the sake of readability!





                    {-
                                ghci> forM [1..3] $ \i -> do print "stupid fucks"; print ((show i) ++ " rahhh"); return (i, i, 1000)
                                "stupid fucks"
                                "1 rahhh"
                                "stupid fucks"
                                "2 rahhh"
                                "stupid fucks"
                                "3 rahhh"
                                [(1,1,1000),(2,2,1000),(3,3,1000)]
                    
                    -}

------- ----- ---- - --------- -- - -- -- ---- -- ---   --  -   -   -   --  ----


{-
-={ bouquet of flowers }=-
        @(\/)
     (\/)-{}-)@
   @(={}=)/\)(\/)
  (\/(/\)@| (-{}-)
 (={}=)@(\/)@(/\)@
  (/\)\(={}=)/(\/)
  @(\/)\(/\)/(={}=)
  (-{}-)""""@/(/\)
   (/\)|:   |
      /::'   \
     /:::     \
    |::'       |
    |::        |
    \::.       /
     ':______.'
      `""""""`
-}
                    -- see monads as just program descriptions... they are for stitching together and wiring + glueing 
                    -- together the wiring of various machines

                    -- monad only needs `return` <-- wrapping value in context ++ `(>>=)` <-- how to glue/wire scripts together!
                                    -- it also makes sense when you consider how superclass Functor and Applicatives are also all about how
                                    -- machines are wired together... e.g. (<$>) and (<*>)


                                -- Ok yeah i can kinda see the program description analogy... that is... viewing list, maybe, reader monad, 
                                -- state monads, and IO too... as more of just a script and them being a monad means they have a monad 
                                -- instance which dictates that one can wrap smth into the current context... and especially having a >>= 
                                -- defined with wires the the machines with more of itself thats different together... yeah this id say is 
                                -- a lot better definition... because just thinking that its a container as well isnt exactly ideal becuase 
                                -- that feels more like a traversable thing then again... and tbf its superclasses... like functors and 
                                -- applicatives are all also all about wiring machines tgt.... so yeah this makes perfect sense in fact


{-
    You have successfully reached the "Enlightenment" stage of learning Monads.

    What you just described--viewing Monads as "Instructions" or "Scripts" 
    rather than as "Containers"--is the exact




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




    THINGS HINTED TO BE NEEDED IN THE EXAM
    - `nub :: Eq a => [a] -> [a]`                       <-- `import Data.List` required
-}





--             mx >>= f             ===                 join (fmap f mx)                === concatMap f mx              <-- do always remind yourself that >>= IS concatMap
--             join mmx             ===                 mmx >>= id