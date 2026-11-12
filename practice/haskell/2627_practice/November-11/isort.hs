module Isort where

import Prelude

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort []     = []
isort [x]    = [x]
isort (x:xs) = insert x $ isort xs

fisort :: Ord a => [a] -> [a]
fisort = foldr insert []

fold_experiment :: (Ord a, Num a) => [a] -> [a]
fold_experiment = foldr (\x acc -> x+5 : acc) [3]

main :: IO()
main = do
  putStrLn $ show $ fold_experiment $ [3, 4, 5]
  putStrLn $ show $ isort [8, 9, 10, 3]
  putStrLn "done"


{-
  [5, 2, 3]

  isort (5:[2, 3])
  x = 5 | xs = [2, 3]
  insert 5 $ isort [2, 3]
    isort [2, 3]
    x = 2 | xs = [3]
    insert 2 $ isort [3] 
    insert 2 [3]
    [2, 3]
  insert 5 [2, 3]
  [2, 3, 5]
-}

{-
  5 [2, 3, 4, 7]
  x = 5 | y = 2 | ys = [3, 4, 7]
  5 > 2 
  
  2 : insert 5 [3, 4, 7]
    x = 5 | y = 3 | ys = [4, 7]
    5 > 3

    3 : insert 5 [4, 7]
      x = 5 | y = 4 | ys = [7]
      5 > 4
      
      4 : insert 5 [7]
        x = 5 | y = 7 | ys = []
        5 <= 7
        5 : 7 : []
-}
