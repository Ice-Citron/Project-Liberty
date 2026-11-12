module Initial where

import Data.List (sort)

sortWrapper :: Ord a => [a] -> [a]
sortWrapper xs = sort xs

main :: IO ()
main = do
  putStrLn $ show $ sortWrapper [5, 4, 3, 2, 7]
  putStrLn "done"
